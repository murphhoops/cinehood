# Phase 2, continued: describe each neighborhood and district, draft names,
# and write the report, the map and the naming sheet.

# Keywords that set a group apart: common inside it and rarer outside it.
# score = share of the group's films carrying the keyword x log(how much more
# common it is in the group than in the whole catalog).
distinctive_keywords <- function(film_groups, film_keywords, vocab, group_col, min_count = 3) {
  n_all <- dplyr::n_distinct(film_groups$tmdb_id)
  sizes <- dplyr::count(film_groups, .data[[group_col]], name = "n_group")
  film_keywords |>
    dplyr::distinct(tmdb_id, keyword_id, keyword) |>
    dplyr::semi_join(vocab, by = "keyword_id") |>
    dplyr::inner_join(film_groups, by = "tmdb_id") |>
    dplyr::count(.data[[group_col]], keyword_id, keyword, name = "n_in") |>
    dplyr::left_join(sizes, by = group_col) |>
    dplyr::left_join(dplyr::select(vocab, keyword_id, df, weight), by = "keyword_id") |>
    dplyr::filter(n_in >= min_count) |>
    dplyr::mutate(share = n_in / n_group,
                  lift = share / (df / n_all),
                  score = share * log(lift) * weight) |>
    dplyr::filter(lift > 1)
}

top_n_text <- function(x, n, sep = " / ") paste(utils::head(x, n), collapse = sep)

# Examples: well-known films at the neighborhood's heart. Core members that fit
# firmly (fit >= 0.6) come first, most-voted first; others only fill in.
example_rank <- function(placement, fit, vote_count) {
  tier <- ifelse(placement == "core" & fit >= 0.6, 0L, ifelse(placement == "core", 1L, 2L))
  order(tier, -vote_count)
}

describe_groups <- function(film_groups, group_col, spine, vocab, n_name_words, n_examples) {
  films <- dplyr::inner_join(film_groups, spine$films, by = "tmdb_id")
  kw <- distinctive_keywords(film_groups, spine$film_keywords, vocab, group_col) |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::arrange(dplyr::desc(score), .by_group = TRUE) |>
    dplyr::summarise(top_keywords = top_n_text(keyword, 8, ", "),
                     draft_name = top_n_text(keyword, n_name_words), .groups = "drop")
  genres <- spine$film_genres |>
    dplyr::inner_join(film_groups, by = "tmdb_id") |>
    dplyr::count(.data[[group_col]], genre) |>
    dplyr::left_join(dplyr::count(film_groups, .data[[group_col]], name = "size"), by = group_col) |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::arrange(dplyr::desc(n), .by_group = TRUE) |>
    dplyr::summarise(top_genres = top_n_text(sprintf("%s %d%%", genre, round(100 * n / size)), 3, ", "),
                     .groups = "drop")
  basics <- films |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      size = dplyr::n(),
      years = sprintf("%d-%d", as.integer(stats::quantile(year, 0.1)), as.integer(stats::quantile(year, 0.9))),
      median_year = as.integer(stats::median(year)),
      top_language = names(sort(table(original_language), decreasing = TRUE))[1],
      top_language_share = round(max(table(original_language)) / dplyr::n(), 2),
      examples = {
        o <- example_rank(placement, fit, vote_count)
        top_n_text(sprintf("%s (%d)", title[o], year[o]), n_examples, "; ")
      },
      core_share = round(mean(placement == "core"), 2),
      mean_fit = round(mean(fit), 3),
      .groups = "drop")
  basics |>
    dplyr::left_join(kw, by = group_col) |>
    dplyr::left_join(genres, by = group_col) |>
    dplyr::mutate(draft_name = dplyr::coalesce(draft_name, top_genres)) |>
    dplyr::arrange(.data[[group_col]])
}

city_tables <- function(city, spine, cfg) {
  fn <- city$film_neighborhoods
  vocab <- keyword_vocab(spine$film_keywords, nrow(spine$films), cfg)
  hoods <- describe_groups(dplyr::select(fn, tmdb_id, neighborhood_id, placement, fit), "neighborhood_id",
                           spine, vocab, n_name_words = 3, n_examples = 8) |>
    dplyr::left_join(dplyr::distinct(fn, neighborhood_id, district_id), by = "neighborhood_id") |>
    dplyr::left_join(fn |> dplyr::group_by(neighborhood_id) |>
                       dplyr::summarise(x = mean(x), y = mean(y), share_placed = round(mean(from_noise), 2)),
                     by = "neighborhood_id") |>
    dplyr::relocate(neighborhood_id, district_id, draft_name, size)
  districts <- describe_groups(dplyr::select(fn, tmdb_id, district_id, placement, fit), "district_id",
                               spine, vocab, n_name_words = 2, n_examples = 6) |>
    dplyr::left_join(dplyr::count(hoods, district_id, name = "n_neighborhoods"), by = "district_id") |>
    dplyr::left_join(fn |> dplyr::group_by(district_id) |> dplyr::summarise(x = mean(x), y = mean(y)),
                     by = "district_id") |>
    dplyr::relocate(district_id, draft_name, n_neighborhoods, size)
  list(neighborhoods = hoods, districts = districts)
}

write_city <- function(city, tabs, cfg) {
  dir.create(cfg$city_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    write_table(city$film_neighborhoods, "film_neighborhoods", cfg$city_dir),
    write_table(city$cluster_space, "cluster_space", cfg$city_dir),
    write_table(tabs$neighborhoods, "neighborhoods", cfg$city_dir),
    write_table(tabs$districts, "districts", cfg$city_dir)
  )
  # A sheet for naming by hand: fill in your_name, leave the rest.
  naming <- tabs$neighborhoods |>
    dplyr::transmute(neighborhood_id, district_id, size, draft_name, years, top_genres,
                     top_keywords, examples, your_name = "")
  sheet <- file.path(cfg$city_dir, "neighborhood_names.csv")
  utils::write.csv(naming, sheet, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  c(paths, sheet)
}

city_map <- function(city, tabs, cfg) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NA_character_)
  dir.create(cfg$check_dir, recursive = TRUE, showWarnings = FALSE)
  pts <- dplyr::mutate(city$film_neighborhoods, district = factor(district_id))
  labs <- dplyr::mutate(tabs$districts, label = paste0(district_id, ". ", draft_name))
  p <- ggplot2::ggplot(pts, ggplot2::aes(x, y, colour = district)) +
    ggplot2::geom_point(size = 0.35, alpha = 0.55) +
    ggplot2::geom_label(data = labs, ggplot2::aes(x, y, label = label), inherit.aes = FALSE,
                        size = 2.6, alpha = 0.85, label.size = 0) +
    ggplot2::guides(colour = "none") +
    ggplot2::theme_void() +
    ggplot2::labs(title = sprintf("Cinehood: %s films in %d neighborhoods, %d districts",
                                  format(nrow(pts), big.mark = ","), nrow(tabs$neighborhoods),
                                  nrow(tabs$districts)))
  path <- file.path(cfg$check_dir, "city_map.png")
  ggplot2::ggsave(path, p, width = 12, height = 10, dpi = 150, bg = "white")
  path
}

city_report <- function(city, tabs, cfg) {
  h <- tabs$neighborhoods
  d <- tabs$districts
  fn <- city$film_neighborhoods
  set.seed(cfg$city_seed)
  review <- h[sort(sample(nrow(h), min(cfg$city_review_n, nrow(h)))), ]
  block <- function(r) c(
    sprintf("### %d. %s", r$neighborhood_id, r$draft_name),
    "",
    sprintf("%s films, %s, district %d, %s. Core %.0f%%, avg fit %.2f", format(r$size, big.mark = ","),
            r$years, r$district_id, dplyr::coalesce(r$top_genres, ""), 100 * r$core_share, r$mean_fit),
    "",
    sprintf("Keywords: %s", dplyr::coalesce(r$top_keywords, "(none distinctive)")),
    "",
    sprintf("Films: %s", r$examples),
    "")
  q <- stats::quantile(h$size, c(0, 0.5, 1))
  lines <- c(
    "# Cinehood phase 2: city report",
    "",
    paste0("Run on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "."),
    "",
    "## Summary",
    "",
    "| | |",
    "| --- | --- |",
    sprintf("| Films placed | %s |", format(nrow(fn), big.mark = ",")),
    sprintf("| Neighborhoods | %d |", nrow(h)),
    sprintf("| Districts | %d |", nrow(d)),
    sprintf("| Neighborhood size: smallest, median, largest | %d, %d, %s |", q[[1]], as.integer(q[[2]]),
            format(q[[3]], big.mark = ",")),
    sprintf("| Neighborhoods under 20 films | %d |", sum(h$size < 20)),
    sprintf("| Neighborhoods over 150 films | %d |", sum(h$size > 150)),
    sprintf("| Films placed in a neighborhood by the clustering (core) | %s (%s) |",
            format(sum(fn$placement == "core"), big.mark = ","), pct(fn$placement == "core")),
    sprintf("| Films the clustering left unclaimed, moved to the nearest neighborhood | %s (%s) |",
            format(sum(fn$placement == "nearest"), big.mark = ","), pct(fn$placement == "nearest")),
    sprintf("| Films with under %d content keywords, placed afterwards as a loose fit | %s (%s) |",
            cfg$city_min_keywords, format(sum(fn$placement == "loose fit"), big.mark = ","),
            pct(fn$placement == "loose fit")),
    sprintf("| Neighborhoods split on a hard genre line (%s) | %d |",
            paste(cfg$city_split_genres, collapse = ", "), city$feature_info$genre_splits %||% 0L),
    sprintf("| Keywords used as features (of which counted at %s weight) | %s (%s) |",
            cfg$city_light_weight, format(city$feature_info$n_keywords, big.mark = ","),
            format(city$feature_info$n_light_keywords, big.mark = ",")),
    "",
    "## Districts",
    "",
    "| # | Draft name | Neighborhoods | Films | Years | Top genres |",
    "| --- | --- | --- | --- | --- | --- |",
    sprintf("| %d | %s | %d | %s | %s | %s |", d$district_id, d$draft_name, d$n_neighborhoods,
            format(d$size, big.mark = ","), d$years, d$top_genres),
    "",
    "## The 10 biggest neighborhoods",
    "",
    "| # | Draft name | Films | Years | Examples |",
    "| --- | --- | --- | --- | --- |",
    {
      b <- utils::head(h[order(-h$size), ], 10)
      sprintf("| %d | %s | %d | %s | %s |", b$neighborhood_id, b$draft_name, b$size, b$years,
              vapply(strsplit(b$examples, "; "), function(e) paste(utils::head(e, 3), collapse = "; "), character(1)))
    },
    "",
    "## Worth a second look",
    "",
    "The 10 neighborhoods whose films sit least firmly in them (lowest average fit). If any feel like a grab bag, these are the likeliest.",
    "",
    "| # | Draft name | Films | Avg fit | Core share | Examples |",
    "| --- | --- | --- | --- | --- | --- |",
    {
      w <- utils::head(h[order(h$mean_fit), ], 10)
      sprintf("| %d | %s | %d | %.2f | %.0f%% | %s |", w$neighborhood_id, w$draft_name, w$size, w$mean_fit,
              100 * w$core_share,
              vapply(strsplit(w$examples, "; "), function(e) paste(utils::head(e, 4), collapse = "; "), character(1)))
    },
    "",
    sprintf("## %d random neighborhoods, for a check by eye", nrow(review)),
    "",
    "Do these read like real micro-genres to a movie fan? Note any that look like a grab bag.",
    "",
    unlist(lapply(seq_len(nrow(review)), function(i) block(review[i, ])))
  )
  dir.create(cfg$check_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cfg$check_dir, "city_report.md")
  writeLines(lines, path, useBytes = TRUE)
  path
}
