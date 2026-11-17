# Glue between the steps, shared by _targets.R and run_phase1.R.

parse_cached <- function(candidates, fetch_log, cfg) {
  # fetch_log is an argument only so the pipeline reparses after a fetch.
  movies <- read_cached_movies(candidates$tmdb_id, cfg$raw_dir)
  parse_movies(movies, cfg$cast_n)
}

write_spine <- function(spine, out_dir) {
  vapply(names(spine), function(n) write_table(spine[[n]], n, out_dir), character(1))
}
