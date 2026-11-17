# Step 1: TMDB's daily ID export -> the candidate list.
# The export lists every movie ID with its current popularity, one JSON object
# per line: {"adult":false,"id":550,"original_title":"Fight Club","popularity":61.4,"video":false}

export_url <- function(date) {
  sprintf("https://files.tmdb.org/p/exports/movie_ids_%s.json.gz",
          format(as.Date(date), "%m_%d_%Y"))
}

default_export_date <- function() {
  as.Date(format(Sys.time(), tz = "UTC")) - 1
}

export_date_of <- function(path) {
  as.Date(sub("^movie_ids_(\\d{2})_(\\d{2})_(\\d{4})\\.json\\.gz$", "\\3-\\1-\\2", basename(path)))
}

# The newest export already on disk, if any, and how old it is in days.
newest_cached_export <- function(dest_dir, today) {
  files <- list.files(dest_dir, pattern = "^movie_ids_\\d{2}_\\d{2}_\\d{4}\\.json\\.gz$", full.names = TRUE)
  if (!length(files)) return(NULL)
  dates <- export_date_of(files)
  i <- which.max(dates)
  list(path = files[[i]], age_days = as.numeric(today - dates[[i]]))
}

# Download to a temporary name and rename only when complete, so a dropped
# connection never leaves a broken file that later runs would trust.
try_download <- function(url, dest, tries = 3, wait_secs = 5) {
  tmp <- paste0(dest, ".part")
  for (k in seq_len(tries)) {
    ok <- tryCatch({
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE, method = "libcurl")
      file.exists(tmp) && file.size(tmp) > 0
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) return(file.rename(tmp, dest))
    unlink(tmp)
    if (k < tries) Sys.sleep(wait_secs)
  }
  FALSE
}

# Which ID export to use:
#   - date set in config: that day's file (downloaded if needed).
#   - otherwise: reuse an export on disk up to max_age_days old, so runs on
#     consecutive days don't re-download and a flaky download doesn't stop a
#     rerun; if none is that recent, try yesterday's, then the two days before.
download_id_export <- function(raw_dir, date = NULL, max_age_days = 7,
                               today = as.Date(format(Sys.time(), tz = "UTC")),
                               downloader = try_download) {
  dest_dir <- file.path(raw_dir, "exports")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(date)) {
    dest <- file.path(dest_dir, basename(export_url(date)))
    if (file.exists(dest) || downloader(export_url(date), dest)) return(dest)
    stop("Couldn't download the TMDB ID export for ", as.Date(date), ".", call. = FALSE)
  }

  cached <- newest_cached_export(dest_dir, today)
  if (!is.null(cached) && cached$age_days <= max_age_days) {
    message("using the ID export already on disk: ", basename(cached$path))
    return(cached$path)
  }

  for (d in as.list(today - 1:3)) {
    dest <- file.path(dest_dir, basename(export_url(d)))
    if (file.exists(dest) || downloader(export_url(d), dest)) return(dest)
    message("no ID export for ", d, "; trying the day before")
  }
  if (!is.null(cached)) {
    message("downloads failed; falling back to ", basename(cached$path))
    return(cached$path)
  }
  stop("Couldn't download a TMDB ID export for any of the last 3 days. ",
       "Check your connection and try again.", call. = FALSE)
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
