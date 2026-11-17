# Load the pipeline's functions and point at the made-up fixture films.
# Fixtures are invented test data, not real TMDB records.
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(root, "R"), full.names = TRUE, pattern = "\\.R$")) source(f)

fixture <- function(name) testthat::test_path("fixtures", name)

load_fixture_movies <- function(ids = 901:904) {
  lapply(ids, function(id) {
    m <- jsonlite::fromJSON(fixture(paste0("movie_", id, ".json")), simplifyVector = FALSE)
    m$.fetched_at <- "2026-11-16"
    m
  })
}

test_cfg <- function(dir) {
  cfg <- cinehood_config()
  cfg$raw_dir   <- file.path(dir, "raw")
  cfg$out_dir   <- file.path(dir, "spine")
  cfg$check_dir <- file.path(dir, "checks")
  cfg
}
