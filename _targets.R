# Phase 1 pipeline: the data spine.
# Run with targets::tar_make(); see what's stale with targets::tar_visnetwork().

library(targets)
tar_option_set(packages = c("dplyr"))
tar_source("R")

list(
  tar_target(cfg, cinehood_config()),

  # 1. Today's list of every TMDB movie ID, and the most popular ones to fetch.
  #    Pinned to the day it first ran; set export_date in R/config.R (or
  #    tar_invalidate(export_file)) to pull a newer list.
  tar_target(export_file, download_id_export(cfg$raw_dir, cfg$export_date), format = "file"),
  tar_target(candidates, select_candidates(read_id_export(export_file), cfg$candidate_top_n)),

  # 2. Details for each candidate, cached as raw JSON under data/raw/movies.
  #    Always checked, so films past the 6-month refresh window get refetched.
  tar_target(fetch_log, fetch_movies(candidates$tmdb_id, cfg), cue = tar_cue(mode = "always")),

  # 3-4. Raw JSON -> tidy tables -> floors and derived fields.
  tar_target(parsed, parse_cached(candidates, fetch_log, cfg)),
  tar_target(spine, derive_spine(parsed, cfg)),

  # 5. Outputs: one file per table in data/spine, plus a report.
  tar_target(spine_files, write_spine(spine, cfg$out_dir), format = "file"),
  tar_target(report, spine_report(spine, candidates, fetch_log, cfg), format = "file")
)
