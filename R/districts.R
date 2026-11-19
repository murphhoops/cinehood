# Hand-picked districts. The data finds neighborhoods; districts are the map
# people browse, so they're chosen by hand, like the sections of a bookstore.
#
# names/districts.csv lists the districts in display order (name, subtitle,
# and optionally claims_genre). Each neighborhood's district is decided, in
# order, by:
#   1. you: the your_district you saved for it (save_names.R)
#   2. default: names/district_defaults.csv, by the neighborhood's name
#   3. genre: a district with claims_genre takes any neighborhood with at
#      least half its films in that genre (The Archive: documentaries)
#   4. nearest: the district of the closest neighborhood decided above
# Without names/districts.csv, districts come from the data as before.

load_district_list <- function(cfg) {
  path <- file.path(cfg$names_dir, "districts.csv")
  if (!file.exists(path)) return(NULL)
  d <- read_sheet(path)
  if (!"subtitle" %in% names(d)) d$subtitle <- ""
  if (!"claims_genre" %in% names(d)) d$claims_genre <- ""
  dplyr::transmute(d, district_id = dplyr::row_number(), name = clean_name(name),
                   subtitle = clean_name(subtitle), claims_genre = clean_name(claims_genre)) |>
    dplyr::filter(nzchar(name))
}

load_district_defaults <- function(cfg) {
  path <- file.path(cfg$names_dir, "district_defaults.csv")
  if (!file.exists(path)) return(dplyr::tibble(neighborhood = character(), district = character()))
  d <- read_sheet(path)
  dplyr::transmute(d, neighborhood = clean_name(neighborhood), district = clean_name(district))
}

assign_districts <- function(city, tabs, spine, cfg) {
  dl <- load_district_list(cfg)
  if (is.null(dl) || !nrow(dl)) return(list(city = city, tabs = tabs))
  h <- tabs$neighborhoods
  fn <- city$film_neighborhoods
  defaults <- load_district_defaults(cfg)

  chosen <- if ("your_district" %in% names(h)) h$your_district else rep("", nrow(h))
  unknown <- setdiff(unique(chosen[nzchar(chosen)]), dl$name)
  if (length(unknown)) {
    warning("Not in names/districts.csv, ignored: ", paste(unknown, collapse = ", "), call. = FALSE)
    chosen[chosen %in% unknown] <- ""
  }
  source <- ifelse(nzchar(chosen), "you", "")

  nm <- if ("your_name" %in% names(h)) h$your_name else rep("", nrow(h))
  by_name <- defaults$district[match(nm, defaults$neighborhood)]
  use <- !nzchar(chosen) & nzchar(nm) & !is.na(by_name) & by_name %in% dl$name
  chosen[use] <- by_name[use]; source[use] <- "default"

  for (i in which(nzchar(dl$claims_genre))) {
    g <- dl$claims_genre[i]
    share <- spine$film_genres |>
      dplyr::filter(genre == g) |>
      dplyr::distinct(tmdb_id) |>
      dplyr::mutate(has = TRUE) |>
      dplyr::right_join(dplyr::select(fn, tmdb_id, neighborhood_id), by = "tmdb_id") |>
      dplyr::group_by(neighborhood_id) |>
      dplyr::summarise(share = mean(!is.na(has)), .groups = "drop")
    s <- share$share[match(h$neighborhood_id, share$neighborhood_id)]
    use <- !nzchar(chosen) & !is.na(s) & s >= 0.5
    chosen[use] <- dl$name[i]; source[use] <- "genre"
  }

  if (any(!nzchar(chosen))) {
    if (!any(nzchar(chosen))) {
      warning("No neighborhood has a district yet; using the data's districts.", call. = FALSE)
      return(list(city = city, tabs = tabs))
    }
    cs <- dplyr::inner_join(dplyr::select(fn, tmdb_id, neighborhood_id), city$cluster_space, by = "tmdb_id")
    pts <- as.matrix(dplyr::select(cs, dplyr::starts_with("u")))
    cents <- centroids_of(pts, cs$neighborhood_id)
    ids <- sort(unique(cs$neighborhood_id))
    row_of <- match(h$neighborhood_id, ids)
    done <- which(nzchar(chosen)); todo <- which(!nzchar(chosen))
    near <- nearest_center(cents[row_of[todo], , drop = FALSE], cents[row_of[done], , drop = FALSE])
    chosen[todo] <- chosen[done][near]; source[todo] <- "nearest"
  }

  h$district_id <- dl$district_id[match(chosen, dl$name)]
  h$your_district <- chosen
  h$district_source <- source
  fn$district_id <- h$district_id[match(fn$neighborhood_id, h$neighborhood_id)]
  city$film_neighborhoods <- fn

  vocab <- keyword_vocab(spine$film_keywords, nrow(spine$films), cfg)
  d <- district_table(fn, h, spine, vocab) |>
    dplyr::left_join(dplyr::select(dl, district_id, your_name = name, your_subtitle = subtitle),
                     by = "district_id")
  tabs$neighborhoods <- h
  tabs$districts <- d
  # District names come from the list now, so saved district names don't apply.
  if (!is.null(tabs$names_unmatched) && nrow(tabs$names_unmatched)) {
    tabs$names_unmatched <- tabs$names_unmatched[tabs$names_unmatched$level != "district", ]
  }
  tabs$district_list <- dl
  list(city = city, tabs = tabs)
}
