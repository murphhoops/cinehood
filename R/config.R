# Phase 1 settings. Change these, then rerun targets::tar_make().
# Cast a wide net at fetch time; cut with the floors below.

cinehood_config <- function() {
  list(
    # Which day's TMDB ID export to use. NULL = yesterday (UTC); TMDB posts each
    # day's file around 08:00 UTC, so "yesterday" is the safe default.
    export_date = NULL,

    # With export_date NULL, reuse an ID export already on disk if it's at
    # most this many days old instead of downloading a new one.
    export_max_age_days = 7,

    # How many of the most popular movie IDs to fetch details for. TMDB counts
    # votes more sparsely than IMDb, so this is deliberately generous.
    candidate_top_n = 40000,

    # The second way in: every film with at least this many votes, searched
    # year by year from discover_first_year on. Kept a little under the vote
    # floors below so films near the line get fetched and checked.
    discover_vote_floor = 200,
    discover_vote_floor_by_era = list(
      list(before = 1970, floor = 50),
      list(before = 1990, floor = 100)
    ),
    discover_first_year = 1900,

    # Floors applied after fetching (the raw fetch keeps everything).
    # Older films get fewer votes on TMDB however well known they are, so the
    # floor drops for earlier eras. vote_floor is the floor from 1990 on, and
    # also the anchor of the weighted rating for every film, so a thinly voted
    # older film is still pulled firmly toward the average.
    vote_floor  = 300,   # TMDB vote_count
    vote_floor_by_era = list(
      list(before = 1970, floor = 75),
      list(before = 1990, floor = 150)
    ),
    min_runtime = 40,    # minutes; drops shorts

    # Billing positions kept from each film's cast list.
    cast_n = 5,

    # Requests per second to the TMDB API, and parallel requests per batch.
    rate_per_sec = 35,
    batch_size   = 20,

    # TMDB's terms: cached data may not be older than 6 months.
    refresh_days = 180,

    # ---- Phase 2: the city (neighborhoods) ----
    # Keywords are the main signal. Keep ones used by at least this many films,
    # and drop ones so common they say little (share of all films).
    city_kw_min_films = 5,
    city_kw_max_share = 0.10,
    # Mood and production keywords are left out of the map automatically, and
    # relationship, place and decade keywords count for less; the lists are in
    # R/keyword_groups.R. Add any other keyword to leave out here.
    city_kw_stoplist = character(),
    city_light_weight = 0.5,   # weight of relationship / place / decade keywords
    # Films with fewer content keywords than this don't help draw the map;
    # they're placed into the nearest neighborhood afterwards, as a loose fit.
    city_min_keywords = 3,
    # How much each kind of feature counts. Directors are deliberately absent:
    # neighborhoods are about what films are like, not who made them.
    # Keywords lead; the others nudge. Checked with tools/check_city_scale.R
    # (14,000 made-up films, 200 planted groups, random genres): at genre 0.35
    # the groups come back whole (98% kept together); at 0.42 that drops to
    # 94% and at 0.5 to 64%. Hard genre lines are handled by the split rule
    # below instead of a heavier genre weight. With horror / animation /
    # documentary assigned to whole groups and the hard-line rule on, the
    # check gives purity 1.00 and 95% kept together.
    city_w_keywords = 1.00,
    city_w_genres   = 0.35,
    city_w_year     = 0.25,
    city_w_language = 0.25,
    city_w_runtime  = 0.10,
    city_top_languages = 12,   # the rest share one "other" column

    city_svd_dims  = 100,  # compress the features before UMAP
    city_neighbors = 30,   # UMAP: how local the structure it keeps is
    city_min_pts   = 15,   # HDBSCAN: smallest group that counts as a neighborhood
    city_districts = 20,   # neighborhoods grouped into this many districts
    # A neighborhood with more core films than this gets a close-up second
    # clustering of its own, and splits if that finds distinct groups.
    city_max_core  = 200,
    # Films outside the clustering's core join the neighborhood where most of
    # their this-many most similar core films live.
    city_place_k   = 15,
    # Genre lines viewers notice more than any keyword overlap. A neighborhood
    # with at least this share on both sides of one is split in two.
    city_split_genres    = c("Horror", "Animation", "Documentary"),
    city_split_min_share = 0.25,
    city_seed      = 2026,
    city_review_n  = 20,   # random neighborhoods printed in the report for a check by eye

    # Names you give neighborhoods and districts (see R/names.R). Kept in the
    # repo, not data/, so reruns can't wipe them. After a rerun a name goes to
    # the group holding at least this share of the films it was given to.
    names_dir = "names",
    names_min_overlap = 0.5,

    # Where everything goes. data/ is gitignored.
    raw_dir   = "data/raw",
    out_dir   = "data/spine",
    city_dir  = "data/city",
    check_dir = "data/checks",

    language = "en-US"
  )
}
