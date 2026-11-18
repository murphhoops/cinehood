# Step 1b: the second way in. TMDB's discover search, year by year, for every
# film with at least discover_vote_floor votes, whatever its popularity today.
# This catches older films that have plenty of votes but little current
# activity, which the popularity-ranked ID export misses.
#
# TMDB returns 20 films a page and at most 500 pages per query, so asking one
# year at a time keeps every query well under that cap.

tmdb_discover_request <- function(year, page, vote_floor, token = tmdb_token(),
                                  language = "en-US") {
  httr2::request("https://api.themoviedb.org/3/discover/movie") |>
    httr2::req_url_query(
      primary_release_year = year,
      `vote_count.gte` = vote_floor,
      sort_by = "vote_count.desc",
      include_adult = "false",
      include_video = "false",
      language = language,
      page = page
    ) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_user_agent("cinehood-pipeline (github.com/murphhoops/cinehood)") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 4,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504))
}

# One page of results -> a tibble of the films on it, plus the page count.
read_discover_page <- function(body) {
  res <- body$results %||% list()
  list(
    total_pages = int(body$total_pages),
    films = dplyr::tibble(
      tmdb_id = vapply(res, function(r) int(r$id), integer(1)),
      original_title = vapply(res, function(r) chr(r$original_title), character(1)),
      discover_votes = vapply(res, function(r) int(r$vote_count), integer(1)),
      discover_popularity = vapply(res, function(r) num(r$popularity), numeric(1))
    )
  )
}

# Every qualifying film for one year, across all its pages.
# `fetch` takes (year, page) and returns the parsed JSON body; it's an argument
# so tests can stand in for the API.
discover_year <- function(year, fetch, max_pages = 500) {
  first <- read_discover_page(fetch(year, 1))
  pages <- min(dplyr::coalesce(first$total_pages, 1L), max_pages)
  rest <- if (pages > 1) lapply(2:pages, function(p) read_discover_page(fetch(year, p))$films)
  dplyr::bind_rows(first$films, rest)
}

discover_by_vote_count <- function(cfg, token = tmdb_token(),
                                   today = as.Date(format(Sys.time(), tz = "UTC")),
                                   fetch = NULL) {
  dir <- file.path(cfg$raw_dir, "discover")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  # Reuse a recent finished sweep, like the ID export, so reruns don't repeat it.
  saved <- list.files(dir, pattern = "^discover_\\d{4}-\\d{2}-\\d{2}\\.rds$", full.names = TRUE)
  if (length(saved)) {
    dates <- as.Date(sub("^discover_(.*)\\.rds$", "\\1", basename(saved)))
    i <- which.max(dates)
    if (as.numeric(today - dates[[i]]) <= cfg$export_max_age_days) {
      message("using the vote-count search already on disk: ", basename(saved[[i]]))
      return(readRDS(saved[[i]]))
    }
  }

  if (is.null(fetch)) {
    fetch <- function(year, page) {
      tmdb_discover_request(year, page, cfg$discover_vote_floor, token, cfg$language) |>
        httr2::req_throttle(rate = cfg$rate_per_sec) |>
        perform_with_retry() |>
        httr2::resp_body_json()
    }
  }

  # Each finished year is saved as it completes, so a dropped connection costs
  # only the year it hit. A partial sweep older than the reuse window starts over.
  partial <- file.path(dir, "partial")
  stamp <- file.path(partial, "started.txt")
  if (file.exists(stamp)) {
    started <- as.Date(readLines(stamp, warn = FALSE)[1])
    if (is.na(started) || as.numeric(today - started) > cfg$export_max_age_days) {
      unlink(partial, recursive = TRUE)
    }
  }
  if (!dir.exists(partial)) {
    dir.create(partial, recursive = TRUE)
    writeLines(as.character(today), stamp)
  }

  years <- seq(cfg$discover_first_year, as.integer(format(today, "%Y")))
  year_file <- function(y) file.path(partial, paste0(y, ".rds"))
  todo <- years[!file.exists(year_file(years))]
  if (length(todo) < length(years)) {
    message("vote-count search: resuming, ", length(years) - length(todo), " years already done")
  }

  failed <- integer()
  for (y in todo) {
    if (y %% 10 == 0) message("vote-count search: ", y)
    res <- tryCatch(discover_year(y, fetch), error = function(e) e)
    if (inherits(res, "error")) {
      message("vote-count search: couldn't finish ", y, " (", conditionMessage(res), ")")
      failed <- c(failed, y)
      next
    }
    saveRDS(res, year_file(y))
  }
  if (length(failed)) {
    stop("The vote-count search couldn't reach TMDB for ", length(failed), " year(s): ",
         paste(failed, collapse = ", "), ". Every other year is saved; run again to finish just those.",
         call. = FALSE)
  }

  out <- dplyr::bind_rows(lapply(year_file(years), readRDS)) |>
    dplyr::distinct(tmdb_id, .keep_all = TRUE)
  saveRDS(out, file.path(dir, paste0("discover_", today, ".rds")))
  unlink(partial, recursive = TRUE)
  out
}

# Both ways in, as one candidate list. `source` records how each film got in.
combine_candidates <- function(popular, discovered) {
  pop <- dplyr::transmute(popular, tmdb_id, original_title, export_popularity, from_popular = TRUE)
  disc <- dplyr::transmute(discovered, tmdb_id, original_title_d = original_title, discover_votes,
                           from_votes = TRUE)
  dplyr::full_join(pop, disc, by = "tmdb_id") |>
    dplyr::mutate(
      original_title = dplyr::coalesce(original_title, original_title_d),
      source = dplyr::case_when(
        from_popular %in% TRUE & from_votes %in% TRUE ~ "both",
        from_popular %in% TRUE ~ "popular",
        TRUE ~ "vote count"
      )
    ) |>
    dplyr::select(tmdb_id, original_title, export_popularity, discover_votes, source)
}
