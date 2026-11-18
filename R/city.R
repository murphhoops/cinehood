# Phase 2: the city. Films -> features -> a map -> neighborhoods -> districts.
#
#   city_features()       what each film is like, as numbers (no directors)
#   city_embed()          compress, then lay films out so similar ones sit close
#   city_neighborhoods()  find the clusters
#   build_city()          places every film, including the keyword-poor ones
#   city_districts()      group neighborhoods into a few larger districts

# ---- Features -------------------------------------------------------------

l2_rows <- function(m) {
  norms <- sqrt(Matrix::rowSums(m^2))
  norms[norms == 0] <- 1
  Matrix::Diagonal(x = 1 / norms) %*% m
}

one_hot <- function(row_index, labels, n_rows) {
  lv <- sort(unique(labels))
  Matrix::sparseMatrix(i = row_index, j = match(labels, lv), x = 1,
                       dims = c(n_rows, length(lv)), dimnames = list(NULL, lv))
}

# Which keywords build the map: content keywords at full weight, generic ones
# (relationships, places, decades a film is set in) at reduced weight, mood and
# production keywords left out (see R/keyword_groups.R). Each must be used by
# enough films to matter and not so many that it says nothing.
keyword_vocab <- function(film_keywords, n_films, cfg) {
  film_keywords |>
    dplyr::distinct(tmdb_id, keyword_id, keyword) |>
    dplyr::count(keyword_id, keyword, name = "df") |>
    dplyr::mutate(kind = classify_keyword(keyword, keyword_id)) |>
    dplyr::filter(kind %in% c("content", "light"),
                  !tolower(keyword) %in% tolower(cfg$city_kw_stoplist),
                  df >= cfg$city_kw_min_films, df / n_films <= cfg$city_kw_max_share) |>
    dplyr::mutate(weight = ifelse(kind == "light", cfg$city_light_weight, 1))
}

city_features <- function(spine, cfg) {
  films <- dplyr::arrange(spine$films, tmdb_id)
  n <- nrow(films)
  row_of <- function(ids) match(ids, films$tmdb_id)

  # Keywords: TF-IDF (each keyword once per film) times the keyword's weight,
  # each film scaled to length 1.
  vocab <- keyword_vocab(dplyr::filter(spine$film_keywords, tmdb_id %in% films$tmdb_id), n, cfg)
  kw <- spine$film_keywords |>
    dplyr::distinct(tmdb_id, keyword_id) |>
    dplyr::filter(keyword_id %in% vocab$keyword_id, tmdb_id %in% films$tmdb_id)
  X_kw <- Matrix::sparseMatrix(i = row_of(kw$tmdb_id), j = match(kw$keyword_id, vocab$keyword_id),
                               x = 1, dims = c(n, nrow(vocab)))
  n_kw <- as.integer(Matrix::rowSums(X_kw))
  n_content <- as.integer(Matrix::rowSums(X_kw[, vocab$kind == "content", drop = FALSE]))
  X_kw <- l2_rows(X_kw %*% Matrix::Diagonal(x = log(n / vocab$df) * vocab$weight))

  # The smaller features, kept apart from the keywords so they can't crowd
  # them out of the compression step (see city_embed).
  g <- dplyr::filter(spine$film_genres, tmdb_id %in% films$tmdb_id)
  X_genre <- as.matrix(l2_rows(one_hot(row_of(g$tmdb_id), g$genre, n)))
  top_lang <- names(sort(table(films$original_language), decreasing = TRUE))[
    seq_len(min(cfg$city_top_languages, dplyr::n_distinct(films$original_language)))]
  lang <- ifelse(films$original_language %in% top_lang, films$original_language, "other")
  X_lang <- as.matrix(one_hot(seq_len(n), lang, n))
  z <- function(v) { s <- stats::sd(v); if (is.na(s) || s == 0) v * 0 else (v - mean(v)) / s }

  side <- cbind(cfg$city_w_genres * X_genre,
                cfg$city_w_language * X_lang,
                year = cfg$city_w_year * z(films$year),
                runtime = cfg$city_w_runtime * z(log(films$runtime)))

  list(X_kw = X_kw, side = side, tmdb_id = films$tmdb_id, vocab = vocab,
       n_keywords = n_kw, n_content_keywords = n_content)
}

# ---- Layout ----------------------------------------------------------------

# Keywords hold the detail (thousands of them, each small), so they're
# compressed on their own (truncated SVD) and set to length 1 per film. The
# side features (genre, language, year, runtime) are then added back at their
# weights. Compressing everything together let the few big side features
# take over and sorted the city by genre and era instead of content.
#
# The layout is learned from the films in `fit` (those with enough keywords),
# then the rest are placed onto it, so films with little to go on can't form
# neighborhoods of their own. UMAP runs twice: a 10-dimensional layout to find
# neighborhoods in, and a 2-dimensional one to draw the city.
city_embed <- function(feat, cfg, fit = rep(TRUE, nrow(feat$X_kw))) {
  X_fit <- feat$X_kw[fit, , drop = FALSE]
  k <- min(cfg$city_svd_dims, nrow(X_fit) - 1, ncol(X_fit) - 1)
  set.seed(cfg$city_seed)
  V <- if (k >= 0.5 * min(dim(X_fit))) {
    svd(as.matrix(X_fit), nu = 0, nv = k)$v          # small data: exact SVD
  } else {
    irlba::irlba(X_fit, nv = k)$v
  }
  Z_kw <- as.matrix(feat$X_kw %*% V)                 # every film, same projection
  len <- sqrt(rowSums(Z_kw^2))
  Z_kw <- cfg$city_w_keywords * Z_kw / ifelse(len > 0, len, 1)
  Z <- cbind(Z_kw, feat$side)

  layout <- function(dims, min_dist) {
    nn <- min(cfg$city_neighbors, sum(fit) - 1)
    set.seed(cfg$city_seed)
    m <- uwot::umap(Z[fit, , drop = FALSE], n_neighbors = nn, n_components = dims,
                    min_dist = min_dist, metric = "cosine", n_threads = 1, n_sgd_threads = 1,
                    ret_model = TRUE)
    out <- matrix(NA_real_, nrow(Z), dims)
    out[fit, ] <- m$embedding
    if (any(!fit)) {
      set.seed(cfg$city_seed)
      out[!fit, ] <- uwot::umap_transform(Z[!fit, , drop = FALSE], m, n_threads = 1, n_sgd_threads = 1)
    }
    out
  }
  list(svd = Z, cluster_space = layout(10, 0), map = layout(2, 0.1))
}

# ---- Neighborhoods ---------------------------------------------------------

centroids_of <- function(points, labels) {
  ids <- sort(unique(labels))
  t(vapply(ids, function(k) colMeans(points[labels == k, , drop = FALSE]), numeric(ncol(points))))
}

# Index of the nearest center (rows of `cents`) for each row of `p`.
nearest_center <- function(p, cents) {
  d2 <- outer(rowSums(p^2), rowSums(cents^2), "+") - 2 * p %*% t(cents)
  max.col(-d2, ties.method = "first")
}

# HDBSCAN finds dense groups and leaves some films unclaimed ("noise"). In a
# city everyone lives somewhere, so each unclaimed film moves to the nearest
# neighborhood, and is flagged so we can see how many did.
city_neighborhoods <- function(points, cfg) {
  hd <- dbscan::hdbscan(points, minPts = cfg$city_min_pts)
  lab <- hd$cluster
  if (all(lab == 0)) stop("No neighborhoods found; try a smaller city_min_pts.", call. = FALSE)
  clustered <- lab > 0
  cents <- centroids_of(points[clustered, , drop = FALSE], lab[clustered])
  cluster_ids <- sort(unique(lab[clustered]))
  if (any(!clustered)) {
    lab[!clustered] <- cluster_ids[nearest_center(points[!clustered, , drop = FALSE], cents)]
  }
  list(label = lab, from_noise = !clustered, membership = hd$membership_prob)
}

# ---- Districts -------------------------------------------------------------

# Group neighborhoods whose centers sit close together into districts.
city_districts <- function(points, neighborhood, cfg) {
  cents <- centroids_of(points, neighborhood)
  n_hoods <- nrow(cents)
  k <- min(cfg$city_districts, n_hoods)
  district_of_hood <- if (k < 2) rep(1L, n_hoods) else
    stats::cutree(stats::hclust(stats::dist(cents), method = "ward.D2"), k = k)
  # Number districts by how many films they hold, largest first.
  films_per <- tapply(rep(1, length(neighborhood)), district_of_hood[neighborhood], sum)
  order_ids <- as.integer(names(sort(films_per, decreasing = TRUE)))
  renum <- stats::setNames(seq_along(order_ids), order_ids)
  unname(renum[as.character(district_of_hood)])
}

# ---- Hard genre lines ------------------------------------------------------

# Some genre differences matter more to viewers than any keyword overlap:
# horror vs. not, animated vs. live action, documentary vs. fiction. A
# neighborhood that is a real mix across one of those lines (at least
# `min_share` on each side, and enough films on each side to stand alone) is
# split in two along it. Returns the new labels and how many splits happened.
split_hard_genres <- function(label, tmdb_id, film_genres, cfg) {
  splits <- 0L
  for (g in cfg$city_split_genres) {
    has_g <- tmdb_id %in% film_genres$tmdb_id[film_genres$genre == g]
    for (L in sort(unique(label))) {
      in_L <- label == L
      n_g <- sum(has_g & in_L); n_other <- sum(!has_g & in_L)
      share <- n_g / sum(in_L)
      if (share >= cfg$city_split_min_share && share <= 1 - cfg$city_split_min_share &&
          n_g >= cfg$city_min_pts && n_other >= cfg$city_min_pts) {
        label[has_g & in_L] <- max(label) + 1L
        splits <- splits + 1L
      }
    }
  }
  list(label = label, splits = splits)
}

# How firmly each film sits in its neighborhood: its distance to the second
# nearest neighborhood center against its distance to its own. 0.5 = halfway
# between two neighborhoods; near 1 = right at the heart of its own.
fit_scores <- function(points, label) {
  ids <- sort(unique(label))
  cents <- centroids_of(points, label)
  d2 <- pmax(outer(rowSums(points^2), rowSums(cents^2), "+") - 2 * points %*% t(cents), 0)
  d <- sqrt(d2)
  own <- d[cbind(seq_along(label), match(label, ids))]
  d[cbind(seq_along(label), match(label, ids))] <- Inf
  second <- if (ncol(d) > 1) apply(d, 1, min) else rep(Inf, length(label))
  ifelse(is.finite(second), second / (own + second), 1)
}

# ---- Everything, as tables -------------------------------------------------

build_city <- function(spine, cfg) {
  feat <- city_features(spine, cfg)

  # Films with too few content keywords to say what they're about are placed
  # onto the map afterwards, into the nearest neighborhood, as a looser fit.
  rich <- feat$n_content_keywords >= cfg$city_min_keywords
  if (sum(rich) < 2 * cfg$city_min_pts) {
    stop("Too few films with ", cfg$city_min_keywords, "+ keywords to build a city.", call. = FALSE)
  }
  emb <- city_embed(feat, cfg, fit = rich)

  hood <- city_neighborhoods(emb$cluster_space[rich, , drop = FALSE], cfg)
  label <- integer(nrow(emb$cluster_space))
  label[rich] <- hood$label
  if (any(!rich)) {
    cents <- centroids_of(emb$cluster_space[rich, , drop = FALSE], hood$label)
    ids <- sort(unique(hood$label))
    label[!rich] <- ids[nearest_center(emb$cluster_space[!rich, , drop = FALSE], cents)]
  }

  # Split neighborhoods that straddle a hard genre line (horror, animation,
  # documentary). Placement is decided before this, so it's unaffected.
  split <- split_hard_genres(label, feat$tmdb_id, spine$film_genres, cfg)
  label <- split$label

  # Number neighborhoods by size, largest first.
  sizes <- sort(table(label), decreasing = TRUE)
  new_id <- stats::setNames(seq_along(sizes), names(sizes))
  neighborhood <- unname(new_id[as.character(label)])
  district_of_hood <- city_districts(emb$cluster_space, neighborhood, cfg)

  placement <- rep("core", length(label))
  placement[rich][hood$from_noise] <- "nearest"
  placement[!rich] <- "loose fit"
  membership <- rep(NA_real_, length(label))
  membership[rich] <- hood$membership

  film_neighborhoods <- dplyr::tibble(
    tmdb_id = feat$tmdb_id,
    neighborhood_id = neighborhood,
    district_id = district_of_hood[neighborhood],
    placement = placement,
    from_noise = placement != "core",
    membership = membership,
    fit = round(fit_scores(emb$cluster_space, neighborhood), 3),
    n_keywords = feat$n_content_keywords,
    x = emb$map[, 1],
    y = emb$map[, 2]
  )
  # The 10-dimensional position too: phase 3 uses it to place new films.
  cluster_space <- dplyr::as_tibble(emb$cluster_space, .name_repair = ~ paste0("u", seq_along(.x))) |>
    dplyr::mutate(tmdb_id = feat$tmdb_id, .before = 1)

  list(film_neighborhoods = film_neighborhoods,
       cluster_space = cluster_space,
       feature_info = list(n_keywords = nrow(feat$vocab),
                           n_light_keywords = sum(feat$vocab$kind == "light"),
                           films_without_keywords = sum(feat$n_keywords == 0),
                           films_loose = sum(!rich),
                           genre_splits = split$splits,
                           n_features = ncol(feat$X_kw) + ncol(feat$side)))
}
