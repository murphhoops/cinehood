# Phase 2: the city. Films -> features -> a map -> neighborhoods -> districts.
#
#   city_features()   what each film is like, as numbers (no directors)
#   city_embed()      compress, then lay films out so similar ones sit close
#   city_neighborhoods()  find the clusters; place every film in one
#   city_districts()  group neighborhoods into a few larger districts

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

# Which keywords describe films well: used by enough films to matter, not so
# many that they say nothing, and not on the stoplist.
keyword_vocab <- function(film_keywords, n_films, cfg) {
  film_keywords |>
    dplyr::distinct(tmdb_id, keyword_id, keyword) |>
    dplyr::filter(!tolower(keyword) %in% tolower(cfg$city_kw_stoplist)) |>
    dplyr::count(keyword_id, keyword, name = "df") |>
    dplyr::filter(df >= cfg$city_kw_min_films, df / n_films <= cfg$city_kw_max_share)
}

city_features <- function(spine, cfg) {
  films <- dplyr::arrange(spine$films, tmdb_id)
  n <- nrow(films)
  row_of <- function(ids) match(ids, films$tmdb_id)

  # Keywords: TF-IDF (each keyword once per film), each film scaled to length 1.
  vocab <- keyword_vocab(dplyr::filter(spine$film_keywords, tmdb_id %in% films$tmdb_id), n, cfg)
  kw <- spine$film_keywords |>
    dplyr::distinct(tmdb_id, keyword_id) |>
    dplyr::filter(keyword_id %in% vocab$keyword_id, tmdb_id %in% films$tmdb_id)
  X_kw <- Matrix::sparseMatrix(i = row_of(kw$tmdb_id), j = match(kw$keyword_id, vocab$keyword_id),
                               x = 1, dims = c(n, nrow(vocab)))
  X_kw <- l2_rows(X_kw %*% Matrix::Diagonal(x = log(n / vocab$df)))

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
       films_without_keywords = sum(Matrix::rowSums(X_kw) == 0))
}

# ---- Layout ----------------------------------------------------------------

# Keywords hold the detail (thousands of them, each small), so they're
# compressed on their own (truncated SVD) and set to length 1 per film. The
# side features (genre, language, year, runtime) are then added back at their
# weights. Compressing everything together let the few big side features
# take over and sorted the city by genre and era instead of content.
# Then UMAP twice: a 10-dimensional layout to find neighborhoods in, and a
# 2-dimensional one to draw the city.
city_embed <- function(feat, cfg) {
  X <- feat$X_kw
  k <- min(cfg$city_svd_dims, nrow(X) - 1, ncol(X) - 1)
  set.seed(cfg$city_seed)
  sv <- if (k >= 0.5 * min(dim(X))) {
    full <- svd(as.matrix(X), nu = k, nv = 0)   # small data: exact SVD
    list(u = full$u, d = full$d[seq_len(k)])
  } else {
    irlba::irlba(X, nv = k)
  }
  Z_kw <- sv$u %*% diag(sv$d, nrow = k)
  len <- sqrt(rowSums(Z_kw^2))
  Z_kw <- cfg$city_w_keywords * Z_kw / ifelse(len > 0, len, 1)
  Z <- cbind(Z_kw, feat$side)

  nn <- min(cfg$city_neighbors, nrow(Z) - 1)
  set.seed(cfg$city_seed)
  for_clusters <- uwot::umap(Z, n_neighbors = nn, n_components = 10, min_dist = 0,
                             metric = "cosine", n_threads = 1, n_sgd_threads = 1)
  set.seed(cfg$city_seed)
  for_map <- uwot::umap(Z, n_neighbors = nn, n_components = 2, min_dist = 0.1,
                        metric = "cosine", n_threads = 1, n_sgd_threads = 1)
  list(svd = Z, cluster_space = for_clusters, map = for_map)
}

# ---- Neighborhoods ---------------------------------------------------------

centroids_of <- function(points, labels) {
  ids <- sort(unique(labels))
  t(vapply(ids, function(k) colMeans(points[labels == k, , drop = FALSE]), numeric(ncol(points))))
}

# HDBSCAN finds dense groups and leaves some films unclaimed ("noise"). In a
# city everyone lives somewhere, so each unclaimed film moves to the nearest
# neighborhood, and is flagged so we can see how many did.
city_neighborhoods <- function(emb, cfg) {
  hd <- dbscan::hdbscan(emb$cluster_space, minPts = cfg$city_min_pts)
  lab <- hd$cluster
  if (all(lab == 0)) stop("No neighborhoods found; try a smaller city_min_pts.", call. = FALSE)

  clustered <- lab > 0
  cents <- centroids_of(emb$cluster_space[clustered, , drop = FALSE], lab[clustered])
  cluster_ids <- sort(unique(lab[clustered]))
  if (any(!clustered)) {
    p <- emb$cluster_space[!clustered, , drop = FALSE]
    d2 <- outer(rowSums(p^2), rowSums(cents^2), "+") - 2 * p %*% t(cents)
    lab[!clustered] <- cluster_ids[max.col(-d2, ties.method = "first")]
  }

  # Number neighborhoods by size, largest first.
  sizes <- sort(table(lab), decreasing = TRUE)
  new_id <- stats::setNames(seq_along(sizes), names(sizes))
  list(neighborhood = unname(new_id[as.character(lab)]),
       from_noise = !clustered,
       membership = hd$membership_prob)
}

# ---- Districts -------------------------------------------------------------

# Group neighborhoods whose centers sit close together into districts.
city_districts <- function(emb, hood, cfg) {
  cents <- centroids_of(emb$cluster_space, hood$neighborhood)
  n_hoods <- nrow(cents)
  k <- min(cfg$city_districts, n_hoods)
  district_of_hood <- if (k < 2) rep(1L, n_hoods) else
    stats::cutree(stats::hclust(stats::dist(cents), method = "ward.D2"), k = k)
  # Number districts by how many films they hold, largest first.
  films_per <- tapply(rep(1, length(hood$neighborhood)), district_of_hood[hood$neighborhood], sum)
  order_ids <- as.integer(names(sort(films_per, decreasing = TRUE)))
  renum <- stats::setNames(seq_along(order_ids), order_ids)
  unname(renum[as.character(district_of_hood)])
}

# ---- Everything, as tables -------------------------------------------------

build_city <- function(spine, cfg) {
  feat <- city_features(spine, cfg)
  emb  <- city_embed(feat, cfg)
  hood <- city_neighborhoods(emb, cfg)
  district_of_hood <- city_districts(emb, hood, cfg)

  film_neighborhoods <- dplyr::tibble(
    tmdb_id = feat$tmdb_id,
    neighborhood_id = hood$neighborhood,
    district_id = district_of_hood[hood$neighborhood],
    from_noise = hood$from_noise,
    membership = hood$membership,
    x = emb$map[, 1],
    y = emb$map[, 2]
  )
  # The 10-dimensional position too: phase 3 uses it to place new films.
  cluster_space <- dplyr::as_tibble(emb$cluster_space, .name_repair = ~ paste0("u", seq_along(.x))) |>
    dplyr::mutate(tmdb_id = feat$tmdb_id, .before = 1)

  list(film_neighborhoods = film_neighborhoods,
       cluster_space = cluster_space,
       feature_info = list(n_keywords = nrow(feat$vocab),
                           films_without_keywords = feat$films_without_keywords,
                           n_features = ncol(feat$X_kw) + ncol(feat$side)))
}
