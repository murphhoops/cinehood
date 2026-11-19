# Names you give neighborhoods and districts. They live in names/ (in the
# repo, not data/), so reruns can't wipe them. Each name remembers the films it
# was given to, its anchors: a neighborhood's core films, or every film in a
# district. After a rerun the neighborhood numbers change, so each name goes to
# whichever new group now holds most of its anchors.
#
# The loop:
#   1. run_phase2.R writes data/city/neighborhood_names.csv (and
#      district_names.csv when districts are drawn by the data), with names
#      and districts already filled in where they carried over.
#   2. Type or change your_name, your_subtitle and your_district in the sheet.
#   3. source("save_names.R") stores them in names/.

names_files <- function(cfg) {
  list(names = file.path(cfg$names_dir, "names.csv"),
       anchors = file.path(cfg$names_dir, "name_anchors.csv"))
}

read_sheet <- function(path) {
  dplyr::as_tibble(utils::read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
                                   check.names = FALSE, na.strings = c("", "NA")))
}

# Blank, missing, or a placeholder in brackets ("(no name yet)") = no name.
clean_name <- function(x) {
  x <- trimws(dplyr::coalesce(as.character(x), ""))
  ifelse(startsWith(x, "("), "", x)
}

empty_store <- function() {
  list(names = dplyr::tibble(level = character(), name_id = integer(), name = character(),
                             subtitle = character(), district = character()),
       anchors = dplyr::tibble(name_id = integer(), tmdb_id = integer()))
}

load_names <- function(cfg) {
  f <- names_files(cfg)
  if (!file.exists(f$names)) return(empty_store())
  nm <- read_sheet(f$names)
  an <- if (file.exists(f$anchors)) read_sheet(f$anchors) else empty_store()$anchors
  if (!"district" %in% names(nm)) nm$district <- ""
  list(names = dplyr::transmute(nm, level = as.character(level), name_id = as.integer(name_id),
                                name = clean_name(name), subtitle = clean_name(subtitle),
                                district = clean_name(district)),
       anchors = dplyr::transmute(an, name_id = as.integer(name_id), tmdb_id = as.integer(tmdb_id)))
}

write_names <- function(store, cfg) {
  f <- names_files(cfg)
  dir.create(cfg$names_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(dplyr::arrange(store$names, level, name_id), f$names, row.names = FALSE,
                   fileEncoding = "UTF-8")
  utils::write.csv(dplyr::arrange(store$anchors, name_id, tmdb_id), f$anchors, row.names = FALSE)
  unlist(f)
}

# Give each saved name of one level to the current group holding the largest
# share of its anchors, one name per group. A name whose best share is under
# `min_overlap` (its group was broken up) is left unmatched. When two named
# groups merge, the name whose films make up more of the new group keeps it
# (best matches go first, ranked by films in common / films in either), so a
# small district folded into a big one doesn't take the big one's name.
match_names <- function(groups, store, level, min_overlap = 0.5) {
  nm <- store$names[store$names$level == level &
                      (nzchar(store$names$name) | nzchar(store$names$district)), ]
  none <- list(matched = dplyr::tibble(group = integer(), name_id = integer(), name = character(),
                                       subtitle = character(), district = character(),
                                       overlap = numeric()),
               unmatched = dplyr::tibble(name_id = integer(), name = character(),
                                         best_group = integer(), overlap = numeric()))
  if (!nrow(nm)) return(none)
  hits <- store$anchors |>
    dplyr::filter(name_id %in% nm$name_id) |>
    dplyr::inner_join(groups, by = "tmdb_id") |>
    dplyr::count(name_id, group) |>
    dplyr::group_by(name_id) |>
    dplyr::mutate(overlap = n / sum(n), n_anchors = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::left_join(dplyr::count(groups, group, name = "group_size"), by = "group") |>
    dplyr::mutate(similarity = n / (n_anchors + group_size - n)) |>
    dplyr::arrange(dplyr::desc(similarity), dplyr::desc(overlap))
  taken_name <- integer(); taken_group <- integer(); keep <- logical(nrow(hits))
  for (i in seq_len(nrow(hits))) {
    if (hits$overlap[i] < min_overlap) next
    if (hits$name_id[i] %in% taken_name || hits$group[i] %in% taken_group) next
    keep[i] <- TRUE
    taken_name <- c(taken_name, hits$name_id[i]); taken_group <- c(taken_group, hits$group[i])
  }
  matched <- hits[keep, ] |>
    dplyr::left_join(nm, by = "name_id") |>
    dplyr::transmute(group, name_id, name, subtitle, district, overlap = round(overlap, 2))
  best <- hits |> dplyr::group_by(name_id) |> dplyr::slice_max(overlap, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  unmatched <- nm |>
    dplyr::filter(!name_id %in% matched$name_id, nzchar(name)) |>
    dplyr::left_join(dplyr::select(best, name_id, best_group = group, overlap), by = "name_id") |>
    dplyr::transmute(name_id, name, best_group, overlap = round(overlap, 2))
  list(matched = matched, unmatched = unmatched)
}

# Adds name_id, your_name, your_subtitle and name_overlap to the neighborhood
# and district tables, and lists names that found no group.
apply_names <- function(city, tabs, cfg) {
  store <- load_names(cfg)
  fn <- city$film_neighborhoods
  add <- function(tab, id_col, level) {
    m <- match_names(dplyr::select(fn, tmdb_id, group = dplyr::all_of(id_col)), store, level,
                     cfg$names_min_overlap)
    out <- tab |>
      dplyr::left_join(dplyr::rename(m$matched, !!id_col := group, your_name = name,
                                     your_subtitle = subtitle, your_district = district,
                                     name_overlap = overlap), by = id_col) |>
      dplyr::mutate(your_name = dplyr::coalesce(your_name, ""),
                    your_subtitle = dplyr::coalesce(your_subtitle, ""),
                    your_district = dplyr::coalesce(your_district, ""))
    list(tab = out, unmatched = dplyr::mutate(m$unmatched, level = level, .before = 1))
  }
  h <- add(tabs$neighborhoods, "neighborhood_id", "neighborhood")
  d <- add(tabs$districts, "district_id", "district")
  tabs$neighborhoods <- h$tab
  tabs$districts <- d$tab
  tabs$names_unmatched <- dplyr::bind_rows(h$unmatched, d$unmatched)
  tabs
}

# Reads the naming sheets from the latest run and stores every name in them,
# with its anchors, in names/. Names saved earlier that aren't in these sheets
# (they found no group this run) are kept. Clearing a carried-over name in the
# sheet deletes it.
save_names <- function(cfg,
                       neighborhood_sheet = file.path(cfg$city_dir, "neighborhood_names.csv"),
                       district_sheet = file.path(cfg$city_dir, "district_names.csv")) {
  fn <- read_table("film_neighborhoods", cfg$city_dir)
  store <- load_names(cfg)
  one_level <- function(sheet, id_col, level, core_only) {
    if (is.null(sheet) || !file.exists(sheet)) return(NULL)
    s <- read_sheet(sheet)
    if (!all(c(id_col, "your_name") %in% names(s))) {
      stop(sheet, " needs the columns ", id_col, " and your_name.", call. = FALSE)
    }
    # The sheet must come from the run now in data/city, or the numbers point
    # at different films.
    if ("size" %in% names(s)) {
      sizes <- table(fn[[id_col]])
      now <- as.integer(sizes[as.character(s[[id_col]])])
      if (any(is.na(now) | now != s$size)) {
        stop(sheet, " doesn't match the latest run (its sizes differ). Use the sheet that ",
             "run_phase2.R wrote last, or rerun it.", call. = FALSE)
      }
    }
    dplyr::tibble(group = as.integer(s[[id_col]]),
                  name_id = if ("name_id" %in% names(s)) as.integer(s$name_id) else NA_integer_,
                  name = clean_name(s$your_name),
                  subtitle = if ("your_subtitle" %in% names(s)) clean_name(s$your_subtitle) else "",
                  district = if ("your_district" %in% names(s)) clean_name(s$your_district) else "",
                  level = level, id_col = id_col, core_only = core_only)
  }
  # With a hand-picked district list (names/districts.csv), districts are
  # named there, not in the district sheet.
  if (!is.null(load_district_list(cfg))) district_sheet <- NULL
  rows <- dplyr::bind_rows(one_level(neighborhood_sheet, "neighborhood_id", "neighborhood", TRUE),
                           one_level(district_sheet, "district_id", "district", FALSE))
  if (!nrow(rows)) stop("No naming sheets found in ", cfg$city_dir, call. = FALSE)

  # Cleared names go; named rows replace whatever their name_id held before.
  touched <- rows$name_id[!is.na(rows$name_id)]
  names_kept <- store$names[!store$names$name_id %in% touched, ]
  anchors_kept <- store$anchors[!store$anchors$name_id %in% touched, ]
  named <- rows[nzchar(rows$name) | nzchar(rows$district), ]
  next_id <- max(c(0L, store$names$name_id)) + 1L
  new <- is.na(named$name_id)
  named$name_id[new] <- seq(next_id, length.out = sum(new))
  anchors <- dplyr::bind_rows(lapply(seq_len(nrow(named)), function(i) {
    r <- named[i, ]
    members <- fn[fn[[r$id_col]] == r$group, ]
    ids <- if (r$core_only) members$tmdb_id[members$placement == "core"] else members$tmdb_id
    if (length(ids) < 5) ids <- members$tmdb_id
    dplyr::tibble(name_id = r$name_id, tmdb_id = as.integer(ids))
  }))
  store <- list(names = dplyr::bind_rows(names_kept,
                                         dplyr::select(named, level, name_id, name, subtitle, district)),
                anchors = dplyr::bind_rows(anchors_kept, anchors))
  paths <- write_names(store, cfg)
  message(sprintf("saved %d neighborhood names, %d neighborhood districts and %d district names to %s",
                  sum(named$level == "neighborhood" & nzchar(named$name)),
                  sum(named$level == "neighborhood" & nzchar(named$district)),
                  sum(named$level == "district"), cfg$names_dir))
  invisible(paths)
}
