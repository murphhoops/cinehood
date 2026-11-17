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
