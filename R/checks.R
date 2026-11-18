# Step 5: a short report on what came out, so problems show before phase 2.

pct <- function(x) sprintf("%.1f%%", 100 * mean(x))

spine_report <- function(spine, candidates, fetch_log, cfg) {
  f <- spine$films
  per_film <- function(tbl) {
    n <- dplyr::count(tbl, tmdb_id)
    dplyr::left_join(dplyr::select(f, tmdb_id), n, by = "tmdb_id") |>
      dplyr::mutate(n = dplyr::coalesce(n, 0L)) |>
      dplyr::pull(n)
  }
  kw  <- per_film(spine$film_keywords)
  dir <- per_film(dplyr::filter(spine$film_people, role == "director"))
  cst <- per_film(dplyr::filter(spine$film_people, role == "cast"))

  fetched_ok <- if (nrow(fetch_log)) sum(fetch_log$status == "200") else NA
  failed     <- if (nrow(fetch_log)) sum(!fetch_log$status %in% c("200", "404")) else 0

  lines <- c(
    "# Cinehood phase 1: spine report",
    "",
    paste0("Run on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "."),
    "",
    "## Counts",
    "",
    "| Step | Films |",
    "| --- | --- |",
    sprintf("| Candidates | %s |", format(nrow(candidates), big.mark = ",")),
    if ("source" %in% names(candidates)) {
      src <- dplyr::count(candidates, source)
      sprintf("| … found by %s | %s |", src$source, format(src$n, big.mark = ","))
    },
    sprintf("| Fetched this run | %s |", if (is.na(fetched_ok)) "0 (all cached)" else format(fetched_ok, big.mark = ",")),
    sprintf("| Still failing after retry | %s |", failed),
    sprintf("| Kept (released, %d+ min, %d+ votes) | %s |", cfg$min_runtime, cfg$vote_floor,
            format(nrow(f), big.mark = ",")),
    if ("source" %in% names(candidates)) {
      kept_src <- dplyr::count(dplyr::semi_join(candidates, f, by = "tmdb_id"), source)
      sprintf("| … kept that came in by %s | %s |", kept_src$source, format(kept_src$n, big.mark = ","))
    },
    "",
    "## Coverage of kept films",
    "",
    "| Field | Films with it |",
    "| --- | --- |",
    sprintf("| IMDb id | %s |", pct(!is.na(f$imdb_id))),
    sprintf("| At least 1 keyword | %s |", pct(kw >= 1)),
    sprintf("| At least 5 keywords | %s |", pct(kw >= 5)),
    sprintf("| A director | %s |", pct(dir >= 1)),
    sprintf("| %d billed cast | %s |", cfg$cast_n, pct(cst >= cfg$cast_n)),
    sprintf("| US certification | %s |", pct(!is.na(f$us_certification))),
    sprintf("| Overview text | %s |", pct(!is.na(f$overview) & nzchar(f$overview))),
    "",
    "## Spread",
    "",
    sprintf("- Years: %d to %d; median %d", min(f$year), max(f$year), as.integer(stats::median(f$year))),
    sprintf("- Votes: median %s, 90th percentile %s",
            format(stats::median(f$vote_count), big.mark = ","),
            format(stats::quantile(f$vote_count, 0.9, names = FALSE), big.mark = ",")),
    sprintf("- Weighted rating: %.2f to %.2f (catalog mean %.2f)",
            min(f$public_wr), max(f$public_wr), mean(f$vote_average)),
    sprintf("- Original language not English: %s", pct(f$original_language != "en")),
    sprintf("- Distinct keywords: %s; used by 5+ films: %s",
            format(nrow(spine$keyword_counts), big.mark = ","),
            format(sum(spine$keyword_counts$n_films >= 5), big.mark = ",")),
    "",
    "## Films per decade",
    "",
    "| Decade | Films |",
    "| --- | --- |",
    {
      d <- dplyr::count(f, decade)
      sprintf("| %s | %s |", d$decade, format(d$n, big.mark = ","))
    },
    "",
    "## Top genres",
    "",
    "| Genre | Films |",
    "| --- | --- |",
    {
      g <- dplyr::count(spine$film_genres, genre, sort = TRUE)
      sprintf("| %s | %s |", g$genre, format(g$n, big.mark = ","))
    }
  )

  dir.create(cfg$check_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cfg$check_dir, "spine_report.md")
  writeLines(lines, path)
  path
}
