# Step 1: TMDB's daily ID export -> the candidate list.
# The export lists every movie ID with its current popularity, one JSON object
# per line: {"adult":false,"id":550,"original_title":"Fight Club","popularity":61.4,"video":false}

export_url <- function(date) {
  sprintf("http://files.tmdb.org/p/exports/movie_ids_%s.json.gz",
          format(as.Date(date), "%m_%d_%Y"))
}

default_export_date <- function() {
  as.Date(format(Sys.time(), tz = "UTC")) - 1
}

download_id_export <- function(raw_dir, date = NULL) {
  date <- if (is.null(date)) default_export_date() else as.Date(date)
  dest_dir <- file.path(raw_dir, "exports")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, basename(export_url(date)))
  if (!file.exists(dest)) {
    utils::download.file(export_url(date), dest, mode = "wb", quiet = TRUE)
  }
  dest
}

read_id_export <- function(path) {
  con <- gzfile(path, open = "r")
  on.exit(close(con))
  ids <- jsonlite::stream_in(con, verbose = FALSE)
  dplyr::as_tibble(ids) |>
    dplyr::transmute(
      tmdb_id = as.integer(id),
      original_title,
      export_popularity = as.numeric(popularity),
      adult = as.logical(adult),
      video = as.logical(video)
    )
}

# Keep real feature films, most popular first. Popularity is TMDB's own
# activity score; it only decides which IDs are worth a detail request.
select_candidates <- function(ids, top_n) {
  ids |>
    dplyr::filter(!adult, !video) |>
    dplyr::arrange(dplyr::desc(export_popularity)) |>
    dplyr::slice_head(n = top_n)
}
