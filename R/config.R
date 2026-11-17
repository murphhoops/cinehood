# Phase 1 settings. Change these, then rerun targets::tar_make().
# Cast a wide net at fetch time; cut with the floors below.

cinehood_config <- function() {
  list(
    # Which day's TMDB ID export to use. NULL = yesterday (UTC); TMDB posts each
    # day's file around 08:00 UTC, so "yesterday" is the safe default.
    export_date = NULL,

    # How many of the most popular movie IDs to fetch details for. TMDB counts
    # votes more sparsely than IMDb, so this is deliberately generous.
    candidate_top_n = 40000,

    # Floors applied after fetching (the raw fetch keeps everything).
    vote_floor  = 300,   # TMDB vote_count
    min_runtime = 40,    # minutes; drops shorts

    # Billing positions kept from each film's cast list.
    cast_n = 5,

    # Requests per second to the TMDB API, and parallel requests per batch.
    rate_per_sec = 35,
    batch_size   = 20,

    # TMDB's terms: cached data may not be older than 6 months.
    refresh_days = 180,

    # Where everything goes. data/ is gitignored.
    raw_dir   = "data/raw",
    out_dir   = "data/spine",
    check_dir = "data/checks",

    language = "en-US"
  )
}
