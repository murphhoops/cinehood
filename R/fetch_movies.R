# Step 2: fetch each candidate's details, caching the raw JSON on disk.
# Reruns only fetch what is missing or older than refresh_days, so the first
# run is the slow one (40k films at ~35/s is roughly 20 minutes) and an
# interrupted run picks up where it stopped.

movie_cache_path <- function(id, raw_dir) {
  file.path(raw_dir, "movies", id %/% 1000, paste0(id, ".json"))
}

not_found_path <- function(raw_dir) file.path(raw_dir, "movies", "_not_found.txt")

read_not_found <- function(raw_dir) {
  p <- not_found_path(raw_dir)
  if (!file.exists(p)) return(integer())
  as.integer(readLines(p, warn = FALSE))
}

# Which IDs still need a request: no cached file, or a file past the refresh
# window. IDs TMDB already answered 404 for are skipped.
ids_to_fetch <- function(ids, raw_dir, refresh_days, now = Sys.time()) {
  paths <- movie_cache_path(ids, raw_dir)
  age_days <- as.numeric(difftime(now, file.mtime(paths), units = "days"))
  stale <- is.na(age_days) | age_days > refresh_days
  setdiff(ids[stale], read_not_found(raw_dir))
}

# Sort one batch of results into saved / not found / retry later.
handle_batch <- function(ids, results, raw_dir) {
  status <- vapply(results, function(r) {
    if (inherits(r, "httr2_response")) as.character(httr2::resp_status(r)) else "error"
  }, character(1))

  for (k in which(status == "200")) {
    path <- movie_cache_path(ids[k], raw_dir)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(httr2::resp_body_string(results[[k]]), path, useBytes = TRUE)
  }
  if (any(status == "404")) {
    dir.create(dirname(not_found_path(raw_dir)), recursive = TRUE, showWarnings = FALSE)
    cat(ids[status == "404"], file = not_found_path(raw_dir), sep = "\n", append = TRUE)
  }
  dplyr::tibble(tmdb_id = ids, status = status)
}

fetch_movies <- function(ids, cfg, token = tmdb_token()) {
  todo <- ids_to_fetch(ids, cfg$raw_dir, cfg$refresh_days)
  message(length(ids) - length(todo), " cached, ", length(todo), " to fetch")
  if (!length(todo)) return(dplyr::tibble(tmdb_id = integer(), status = character()))

  batches <- split(todo, ceiling(seq_along(todo) / cfg$batch_size))
  min_secs <- cfg$batch_size / cfg$rate_per_sec
  log <- vector("list", length(batches))

  for (b in seq_along(batches)) {
    started <- Sys.time()
    reqs <- lapply(batches[[b]], tmdb_movie_request, token = token, language = cfg$language)
    res <- httr2::req_perform_parallel(reqs, on_error = "continue", progress = FALSE)
    log[[b]] <- handle_batch(batches[[b]], res, cfg$raw_dir)

    if (b %% 50 == 0) message("batch ", b, " of ", length(batches))
    wait <- min_secs - as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (wait > 0) Sys.sleep(wait)
  }
  log <- dplyr::bind_rows(log)

  # Second pass, one at a time with backoff, for rate limits and server hiccups.
  retry <- log$tmdb_id[!log$status %in% c("200", "404")]
  if (length(retry)) {
    message("retrying ", length(retry), " one at a time")
    for (id in retry) {
      req <- tmdb_movie_request(id, token = token, language = cfg$language) |>
        httr2::req_retry(max_tries = 4, is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504))
      res <- tryCatch(perform_with_retry(req), error = function(e) e)
      log <- dplyr::rows_update(log, handle_batch(id, list(res), cfg$raw_dir), by = "tmdb_id")
    }
  }
  log
}

# Every cached JSON for the candidate IDs, read back in.
read_cached_movies <- function(ids, raw_dir) {
  paths <- movie_cache_path(ids, raw_dir)
  have <- file.exists(paths)
  lapply(paths[have], function(p) {
    m <- jsonlite::fromJSON(p, simplifyVector = FALSE)
    m$.fetched_at <- format(file.mtime(p), "%Y-%m-%d")
    m
  })
}
