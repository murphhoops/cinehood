# The same pipeline as _targets.R, as a plain script, for a first run or if
# you'd rather not use {targets}. From the project folder:
#   source("run_phase1.R")

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

cfg <- cinehood_config()

message("1/5 candidates: most popular, plus every film with enough votes")
export_file <- download_id_export(cfg$raw_dir, cfg$export_date, cfg$export_max_age_days)
popular     <- select_candidates(read_id_export(export_file), cfg$candidate_top_n)
discovered  <- discover_by_vote_count(cfg)
candidates  <- combine_candidates(popular, discovered)

message("2/5 fetching details (cached; first run is the slow one)")
fetch_log <- fetch_movies(candidates$tmdb_id, cfg)

message("3/5 parsing")
parsed <- parse_cached(candidates, fetch_log, cfg)

message("4/5 floors and derived fields")
spine <- derive_spine(parsed, cfg)

message("5/5 writing tables and report")
write_spine(spine, cfg$out_dir)
report <- spine_report(spine, candidates, fetch_log, cfg)
message("done: ", nrow(spine$films), " films. report at ", report)
