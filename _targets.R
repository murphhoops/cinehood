# Phase 1 pipeline: the data spine.
# Run with targets::tar_make(); see what's stale with targets::tar_visnetwork().

library(targets)
tar_option_set(packages = c("dplyr"))
tar_source("R")

list(
  tar_target(cfg, cinehood_config()),

  # 1. Candidates, two ways in: the most popular films in TMDB's daily ID
  #    export, plus every film with enough votes, searched year by year.
  #    Both reuse what's on disk if under export_max_age_days old.
  tar_target(export_file, download_id_export(cfg$raw_dir, cfg$export_date, cfg$export_max_age_days), format = "file"),
  tar_target(popular, select_candidates(read_id_export(export_file), cfg$candidate_top_n)),
  tar_target(discovered, discover_by_vote_count(cfg)),
  tar_target(candidates, combine_candidates(popular, discovered)),

  # 2. Details for each candidate, cached as raw JSON under data/raw/movies.
  #    Always checked, so films past the 6-month refresh window get refetched.
  tar_target(fetch_log, fetch_movies(candidates$tmdb_id, cfg), cue = tar_cue(mode = "always")),

  # 3-4. Raw JSON -> tidy tables -> floors and derived fields.
  tar_target(parsed, parse_cached(candidates, fetch_log, cfg)),
  tar_target(spine, derive_spine(parsed, cfg)),

  # 5. Outputs: one file per table in data/spine, plus a report.
  tar_target(spine_files, write_spine(spine, cfg$out_dir), format = "file"),
  tar_target(report, spine_report(spine, candidates, fetch_log, cfg), format = "file"),

  # Phase 2: the city. Neighborhoods and districts from the spine.
  tar_target(city, build_city(spine, cfg)),
  tar_target(city_tabs, city_tables(city, spine, cfg)),
  tar_target(city_files, write_city(city, city_tabs, cfg), format = "file"),
  tar_target(city_map_file, city_map(city, city_tabs, cfg), format = "file"),
  tar_target(city_report_file, city_report(city, city_tabs, cfg), format = "file")
)
