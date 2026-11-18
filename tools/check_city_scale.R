# Checks the city method at full size: a made-up catalog of 14,000 films in 200
# planted groups, with random genres and languages, random mood tags on 30% of
# films (which should be ignored), and 1,000 extra keyword-poor films (which
# should be placed with their own group as a loose fit). Good settings recover
# the groups: purity and "kept together" near 1.00. From the project folder:
#   Rscript tools/check_city_scale.R            (about 3 minutes)

suppressWarnings(for (f in list.files("R", full.names = TRUE)) source(f))
set.seed(1)
n_groups <- 200; per <- 70; n_rich <- n_groups * per; n_thin <- 1000
grp <- c(rep(seq_len(n_groups), each = per), sample(n_groups, n_thin, TRUE))
n <- length(grp)
films <- dplyr::tibble(tmdb_id = seq_len(n), title = paste("Film", seq_len(n)),
  year = as.integer(pmin(2026, round(rnorm(n, 1960 + (grp %% 60), 6)))),
  original_language = sample(c("en","en","en","fr","ja","ko","es"), n, TRUE),
  runtime = as.integer(round(rnorm(n, 110, 15))), vote_count = sample(75:20000, n, TRUE))
own_n <- c(rep(6L, n_rich), sample(0:2, n_thin, TRUE))
kw <- dplyr::bind_rows(lapply(seq_len(n), function(i) {
  own <- own_n[i]
  dplyr::tibble(tmdb_id = i, keyword = c(if (own) paste0("g", grp[i], "_", sample(1:20, own)),
                                         paste0("shared", sample(1:3000, 4))))
}))
kw$keyword_id <- match(kw$keyword, unique(kw$keyword))
moods <- dplyr::tibble(tmdb_id = sample(n, round(0.3 * n)), keyword = "admiring", keyword_id = 325761L)
kw <- dplyr::bind_rows(kw, moods)
genres <- dplyr::tibble(tmdb_id = seq_len(n), genre = sample(c("Drama","Comedy","Thriller","Action","Horror","Romance","Crime"), n, TRUE))
spine <- list(films = films, film_keywords = kw, film_genres = genres)
cfg <- cinehood_config()
t1 <- system.time(city <- build_city(spine, cfg))[["elapsed"]]
fn <- dplyr::inner_join(city$film_neighborhoods, dplyr::tibble(tmdb_id = seq_len(n), grp = grp), by = "tmdb_id")
rich <- fn[fn$tmdb_id <= n_rich, ]
purity <- rich |> dplyr::count(neighborhood_id, grp) |> dplyr::group_by(neighborhood_id) |>
  dplyr::summarise(t = max(n), s = sum(n)) |> dplyr::summarise(sum(t) / sum(s)) |> dplyr::pull()
together <- rich |> dplyr::count(grp, neighborhood_id) |> dplyr::group_by(grp) |>
  dplyr::summarise(m = max(n) / sum(n)) |> dplyr::summarise(mean(m)) |> dplyr::pull()
home <- rich |> dplyr::count(grp, neighborhood_id) |> dplyr::group_by(grp) |> dplyr::slice_max(n, n = 1, with_ties = FALSE)
thin <- fn[fn$tmdb_id > n_rich & own_n[fn$tmdb_id] >= 1, ] |>
  dplyr::left_join(dplyr::select(home, grp, home_id = neighborhood_id), by = "grp")
thin_ok <- mean(thin$neighborhood_id == thin$home_id)
cat(sprintf("%.0fs | neighborhoods %d (planted 200) | purity %.2f | kept together %.2f | unclaimed %.1f%% | keyword-poor films placed with their group %.2f (of those with 1+ own keyword)\n",
  t1, dplyr::n_distinct(fn$neighborhood_id), purity, together,
  100 * mean(rich$placement == "nearest"), thin_ok))
