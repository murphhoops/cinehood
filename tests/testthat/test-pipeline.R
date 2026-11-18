test_that("the ID export is read and filtered to non-adult, non-video, most popular first", {
  ids <- read_id_export(fixture("movie_ids_test.json.gz"))
  expect_equal(nrow(ids), 5)
  cand <- select_candidates(ids, top_n = 2)
  expect_equal(cand$tmdb_id, c(901L, 902L))
})

test_that("films parse with the right fields and types", {
  p <- parse_movies(load_fixture_movies(), cast_n = 5)
  f <- p$films
  expect_equal(nrow(f), 4)
  one <- f[f$tmdb_id == 901, ]
  expect_equal(one$release_date, as.Date("2012-11-16"))
  expect_equal(one$us_certification, "R")          # skips the blank US entry
  expect_true(is.na(f$imdb_id[f$tmdb_id == 902]))  # null -> NA
  expect_equal(f$collection_name[f$tmdb_id == 902], "Test Collection")
  expect_true(is.na(f$release_date[f$tmdb_id == 903]))  # "" -> NA
})

test_that("people: directors, deduplicated writers, top-billed cast", {
  people <- parse_movies(load_fixture_movies(), cast_n = 5)$film_people
  one <- people[people$tmdb_id == 901, ]
  expect_equal(sum(one$role == "director"), 1)
  expect_equal(sort(one$name[one$role == "writer"]), c("Director X", "Writer Y"))
  cast <- one[one$role == "cast", ]
  expect_equal(nrow(cast), 5)
  expect_equal(cast$billing, 1:5)
  expect_false("Actor F" %in% cast$name)
  expect_equal(sum(people$tmdb_id == 902 & people$role == "director"), 2)
})

test_that("a film with no credits, genres or keywords parses without error", {
  p <- parse_movies(load_fixture_movies(903), cast_n = 5)
  expect_equal(nrow(p$films), 1)
  expect_equal(nrow(p$film_people), 0)
  expect_named(p$film_keywords, c("tmdb_id", "keyword_id", "keyword"))
})

test_that("floors drop unreleased, short and thin films; side tables follow", {
  cfg <- cinehood_config()
  spine <- derive_spine(parse_movies(load_fixture_movies(), cfg$cast_n), cfg)
  expect_setequal(spine$films$tmdb_id, c(901L, 902L))
  expect_true(all(spine$film_keywords$tmdb_id %in% c(901L, 902L)))
  expect_false(105L %in% spine$film_keywords$keyword_id)   # the short's keyword
  expect_equal(spine$keyword_counts$n_films[spine$keyword_counts$keyword == "family"], 2)
  expect_equal(spine$films$decade, c("2010s", "1980s"))
  expect_equal(spine$films$runtime_bucket, c("2h-2h15", "1h30-1h45"))
})

test_that("weighted rating pulls thin films toward the mean", {
  expect_equal(weighted_rating(8, 300, 300, 6), 7)
  expect_equal(weighted_rating(8, 30000, 300, 6), 8 * 30000 / 30300 + 6 * 300 / 30300)
  cfg <- cinehood_config()
  f <- derive_spine(parse_movies(load_fixture_movies(), cfg$cast_n), cfg)$films
  thin <- f[f$tmdb_id == 902, ]
  expect_true(abs(thin$public_wr - mean(f$vote_average)) < abs(thin$vote_average - mean(f$vote_average)))
  expect_equal(mean(f$public_z), 0, tolerance = 1e-9)
})

test_that("the fetch skips cached, fresh files and known 404s", {
  dir <- withr::local_tempdir()
  p901 <- movie_cache_path(901, dir); dir.create(dirname(p901), recursive = TRUE)
  writeLines("{}", p901)
  p902 <- movie_cache_path(902, dir); writeLines("{}", p902)
  Sys.setFileTime(p902, Sys.time() - 200 * 86400)   # past the refresh window
  writeLines("903", not_found_path(dir))
  expect_setequal(ids_to_fetch(c(901, 902, 903, 904), dir, refresh_days = 180), c(902, 904))
})

test_that("a batch saves 200s, logs 404s and flags the rest for retry", {
  dir <- withr::local_tempdir()
  ok <- httr2::response(status_code = 200, body = charToRaw('{"id":901}'))
  gone <- httr2::response(status_code = 404)
  busy <- httr2::response(status_code = 429)
  log <- handle_batch(c(901, 902, 903, 904), list(ok, gone, busy, simpleError("timeout")), dir)
  expect_equal(log$status, c("200", "404", "429", "error"))
  expect_true(file.exists(movie_cache_path(901, dir)))
  expect_equal(read_not_found(dir), 902L)
})

test_that("tables round-trip and the report writes", {
  dir <- withr::local_tempdir()
  cfg <- test_cfg(dir)
  spine <- derive_spine(parse_movies(load_fixture_movies(), cfg$cast_n), cfg)
  paths <- write_spine(spine, cfg$out_dir)
  expect_true(all(file.exists(paths)))
  expect_equal(read_table("films", cfg$out_dir), spine$films)
  cand <- select_candidates(read_id_export(fixture("movie_ids_test.json.gz")), 10)
  log <- dplyr::tibble(tmdb_id = c(901L, 902L), status = c("200", "200"))
  rep <- spine_report(spine, cand, log, cfg)
  txt <- readLines(rep)
  expect_true(any(grepl("Kept \\(released", txt)))
  expect_true(any(grepl("\\| 1980s \\| 1 \\|", txt)))
})

test_that("a recent export on disk is reused instead of downloading", {
  dir <- withr::local_tempdir()
  ex <- file.path(dir, "exports"); dir.create(ex)
  file.copy(fixture("movie_ids_test.json.gz"), file.path(ex, "movie_ids_11_14_2026.json.gz"))
  calls <- 0
  never <- function(url, dest) { calls <<- calls + 1; FALSE }
  p <- download_id_export(dir, today = as.Date("2026-11-17"), downloader = never)
  expect_equal(basename(p), "movie_ids_11_14_2026.json.gz")
  expect_equal(calls, 0)
})

test_that("a failed download falls back to the day before", {
  dir <- withr::local_tempdir()
  tried <- character()
  second_works <- function(url, dest) {
    tried <<- c(tried, basename(url))
    if (length(tried) < 2) return(FALSE)
    file.copy(fixture("movie_ids_test.json.gz"), dest)
  }
  p <- download_id_export(dir, today = as.Date("2026-11-17"), downloader = second_works)
  expect_equal(tried, c("movie_ids_11_16_2026.json.gz", "movie_ids_11_15_2026.json.gz"))
  expect_equal(basename(p), "movie_ids_11_15_2026.json.gz")
})

test_that("an old export is only a last resort, and an outright failure stops clearly", {
  dir <- withr::local_tempdir()
  ex <- file.path(dir, "exports"); dir.create(ex)
  file.copy(fixture("movie_ids_test.json.gz"), file.path(ex, "movie_ids_10_01_2026.json.gz"))
  never <- function(url, dest) FALSE
  p <- download_id_export(dir, today = as.Date("2026-11-17"), downloader = never)
  expect_equal(basename(p), "movie_ids_10_01_2026.json.gz")
  expect_error(download_id_export(withr::local_tempdir(), today = as.Date("2026-11-17"), downloader = never),
               "any of the last 3 days")
})

test_that("a dropped download leaves no partial file behind", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "movie_ids_11_16_2026.json.gz")
  ok <- try_download("https://127.0.0.1:9/nothing.gz", dest, tries = 1, wait_secs = 0)
  expect_false(ok)
  expect_false(file.exists(dest))
  expect_false(file.exists(paste0(dest, ".part")))
})

# A stand-in for TMDB's discover search: `per_year` films a year, 20 a page.
fake_discover <- function(per_year) {
  calls <- 0
  fetch <- function(year, page) {
    calls <<- calls + 1
    n <- per_year[[as.character(year)]] %||% 0
    ids <- if (n) seq_len(n) + year * 1000 else integer()
    on_page <- ids[((page - 1) * 20 + 1):min(page * 20, max(n, 1))]
    on_page <- on_page[!is.na(on_page)]
    list(total_pages = max(1, ceiling(n / 20)),
         results = lapply(on_page, function(i) list(id = i, original_title = paste("Film", i),
                                                     vote_count = 500, popularity = 1.5)))
  }
  list(fetch = fetch, calls = function() calls)
}

test_that("the vote-count search walks every page of a year", {
  f <- fake_discover(list("1960" = 45))
  films <- discover_year(1960, f$fetch)
  expect_equal(nrow(films), 45)
  expect_equal(f$calls(), 3)               # pages of 20, 20 and 5
  expect_false(anyDuplicated(films$tmdb_id) > 0)
})

test_that("the search covers every year, saves itself, and is reused next time", {
  dir <- withr::local_tempdir()
  cfg <- test_cfg(dir)
  cfg$discover_first_year <- 2024
  f <- fake_discover(list("2024" = 3, "2025" = 25, "2026" = 0))
  out <- discover_by_vote_count(cfg, token = "x", today = as.Date("2026-11-17"), fetch = f$fetch)
  expect_equal(nrow(out), 28)
  expect_true(file.exists(file.path(cfg$raw_dir, "discover", "discover_2026-11-17.rds")))
  before <- f$calls()
  again <- discover_by_vote_count(cfg, token = "x", today = as.Date("2026-11-20"), fetch = f$fetch)
  expect_equal(f$calls(), before)          # reused, no new requests
  expect_equal(again, out)
})

test_that("the search request asks for the right films", {
  req <- tmdb_discover_request(1960, 2, 200, token = "x")
  q <- httr2::url_parse(req$url)$query
  expect_equal(q$primary_release_year, "1960")
  expect_equal(q$`vote_count.gte`, "200")
  expect_equal(q$sort_by, "vote_count.desc")
  expect_equal(q$include_adult, "false")
  expect_equal(q$page, "2")
})

test_that("both ways in combine into one list, labeled by source", {
  popular <- dplyr::tibble(tmdb_id = c(1L, 2L), original_title = c("A", "B"),
                           export_popularity = c(50, 40), adult = FALSE, video = FALSE)
  discovered <- dplyr::tibble(tmdb_id = c(2L, 3L), original_title = c("B", "C"),
                              discover_votes = c(900L, 400L), discover_popularity = c(1, 2))
  cand <- combine_candidates(popular, discovered)
  expect_equal(cand$tmdb_id, c(1L, 2L, 3L))
  expect_equal(cand$source, c("popular", "both", "vote count"))
  expect_equal(cand$original_title, c("A", "B", "C"))
})

test_that("the report breaks candidates and kept films down by source", {
  dir <- withr::local_tempdir()
  cfg <- test_cfg(dir)
  spine <- derive_spine(parse_movies(load_fixture_movies(), cfg$cast_n), cfg)
  cand <- dplyr::tibble(tmdb_id = c(901L, 902L, 903L), original_title = "x", export_popularity = 1,
                        discover_votes = 1L, source = c("both", "vote count", "popular"))
  txt <- readLines(spine_report(spine, cand, dplyr::tibble(tmdb_id = integer(), status = character()), cfg))
  expect_true(any(grepl("found by vote count \\| 1 \\|", txt)))
  expect_true(any(grepl("kept that came in by both \\| 1 \\|", txt)))
})

test_that("a dropped connection is retried, and gives up cleanly after the last try", {
  n <- 0
  flaky <- function(req) { n <<- n + 1; if (n < 3) stop("Recv failure: Connection was reset") else "ok" }
  expect_equal(suppressMessages(perform_with_retry("req", perform = flaky, sleep = function(s) NULL)), "ok")
  expect_equal(n, 3)
  dead <- function(req) stop("Connection was reset")
  expect_error(suppressMessages(perform_with_retry("req", tries = 2, perform = dead, sleep = function(s) NULL)),
               "Connection was reset")
})

test_that("a failed year stops the search with a clear note, and a rerun finishes only that year", {
  dir <- withr::local_tempdir()
  cfg <- test_cfg(dir)
  cfg$discover_first_year <- 2024
  good <- fake_discover(list("2024" = 3, "2025" = 25, "2026" = 2))
  broken_2025 <- function(year, page) if (year == 2025) stop("Connection was reset") else good$fetch(year, page)
  expect_error(suppressMessages(
    discover_by_vote_count(cfg, token = "x", today = as.Date("2026-11-17"), fetch = broken_2025)),
    "1 year\\(s\\): 2025")
  partial <- file.path(cfg$raw_dir, "discover", "partial")
  expect_true(file.exists(file.path(partial, "2024.rds")))
  expect_true(file.exists(file.path(partial, "2026.rds")))

  asked <- integer()
  watch <- function(year, page) { asked <<- c(asked, year); good$fetch(year, page) }
  out <- suppressMessages(discover_by_vote_count(cfg, token = "x", today = as.Date("2026-11-17"), fetch = watch))
  expect_equal(unique(asked), 2025)        # only the missing year
  expect_equal(nrow(out), 30)
  expect_false(dir.exists(partial))        # cleaned up once complete
})

test_that("a partial search older than the reuse window starts over", {
  dir <- withr::local_tempdir()
  cfg <- test_cfg(dir)
  cfg$discover_first_year <- 2025
  partial <- file.path(cfg$raw_dir, "discover", "partial")
  dir.create(partial, recursive = TRUE)
  writeLines("2026-10-01", file.path(partial, "started.txt"))
  saveRDS(dplyr::tibble(tmdb_id = 1L, original_title = "stale", discover_votes = 1L, discover_popularity = 1),
          file.path(partial, "2025.rds"))
  f <- fake_discover(list("2025" = 2, "2026" = 1))
  out <- suppressMessages(discover_by_vote_count(cfg, token = "x", today = as.Date("2026-11-17"), fetch = f$fetch))
  expect_false("stale" %in% out$original_title)
  expect_equal(nrow(out), 3)
})
