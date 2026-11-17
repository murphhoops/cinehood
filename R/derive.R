# Step 4: apply the floors, add derived fields, and trim the side tables to the
# films that made the cut.

# TMDB's vote_average is noisy at low vote counts, so the public baseline is a
# weighted rating that pulls thin films toward the catalog mean:
#   WR = v/(v+m) * R + m/(v+m) * C
# with m = the vote floor and C = the mean rating of the kept films.
weighted_rating <- function(r, v, m, c) (v / (v + m)) * r + (m / (v + m)) * c

runtime_bucket <- function(minutes) {
  cut(minutes,
      breaks = c(-Inf, 89, 104, 119, 134, 149, Inf),
      labels = c("under 1h30", "1h30-1h45", "1h45-2h", "2h-2h15", "2h15-2h30", "2h30+"))
}

derive_spine <- function(parsed, cfg) {
  films <- parsed$films |>
    dplyr::filter(
      status == "Released",
      !is.na(release_date),
      !is.na(runtime), runtime >= cfg$min_runtime,
      !is.na(vote_count), vote_count >= cfg$vote_floor
    )

  c_mean <- mean(films$vote_average)
  films <- films |>
    dplyr::mutate(
      year           = as.integer(format(release_date, "%Y")),
      decade         = paste0(year %/% 10 * 10, "s"),
      runtime_bucket = as.character(runtime_bucket(runtime)),
      log_votes      = log10(vote_count),
      public_wr      = weighted_rating(vote_average, vote_count, cfg$vote_floor, c_mean),
      public_z       = as.numeric(scale(public_wr))
    ) |>
    dplyr::arrange(dplyr::desc(vote_count))

  keep <- films$tmdb_id
  side <- c("film_genres", "film_people", "film_keywords", "film_companies", "film_countries")
  out <- list(films = films)
  for (t in side) out[[t]] <- dplyr::filter(parsed[[t]], tmdb_id %in% keep)

  # How many kept films carry each keyword: the clustering step will drop rare
  # ones, and this table makes that cut visible.
  out$keyword_counts <- out$film_keywords |>
    dplyr::count(keyword_id, keyword, name = "n_films", sort = TRUE)
  out
}
