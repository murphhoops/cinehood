# Checks the city method at full size: a made-up catalog of 14,000 films in 200
# planted groups, with random genres and languages. Good settings recover the
# groups (purity and "kept together" near 1.00). Usage, from the project folder:
#   Rscript tools/check_city_scale.R <genre weight> <language weight> <year weight>
# e.g. Rscript tools/check_city_scale.R 0.35 0.25 0.25   (about 2-3 minutes)

suppressWarnings(for (f in list.files("R", full.names = TRUE)) source(f))
args <- commandArgs(TRUE)
w_genre <- as.numeric(args[1]); w_lang <- as.numeric(args[2]); w_year <- as.numeric(args[3])
set.seed(1)
n_groups <- 200; per <- 70; n <- n_groups * per
grp <- rep(seq_len(n_groups), each = per)
films <- dplyr::tibble(tmdb_id = seq_len(n), title = paste("Film", seq_len(n)),
  year = as.integer(pmin(2026, round(rnorm(n, 1960 + (grp %% 60), 6)))),
  original_language = sample(c("en","en","en","fr","ja","ko","es"), n, TRUE),
  runtime = as.integer(round(rnorm(n, 110, 15))), vote_count = sample(75:20000, n, TRUE))
kw <- dplyr::bind_rows(lapply(seq_len(n), function(i) dplyr::tibble(tmdb_id = i,
  keyword = c(paste0("g", grp[i], "_", sample(1:20, 6)), paste0("shared", sample(1:3000, 4))))))
kw$keyword_id <- match(kw$keyword, unique(kw$keyword))
genres <- dplyr::tibble(tmdb_id = seq_len(n), genre = sample(c("Drama","Comedy","Thriller","Action","Horror","Romance","Crime"), n, TRUE))
spine <- list(films = films, film_keywords = kw, film_genres = genres)
cfg <- cinehood_config(); cfg$city_w_genres <- w_genre; cfg$city_w_language <- w_lang; cfg$city_w_year <- w_year
t1 <- system.time(city <- build_city(spine, cfg))[["elapsed"]]
fn <- dplyr::inner_join(city$film_neighborhoods, dplyr::tibble(tmdb_id = seq_len(n), grp = grp), by = "tmdb_id")
purity <- fn |> dplyr::count(neighborhood_id, grp) |> dplyr::group_by(neighborhood_id) |> dplyr::summarise(t = max(n), s = sum(n)) |> dplyr::summarise(sum(t)/sum(s)) |> dplyr::pull()
together <- fn |> dplyr::count(grp, neighborhood_id) |> dplyr::group_by(grp) |> dplyr::summarise(m = max(n)/sum(n)) |> dplyr::summarise(mean(m)) |> dplyr::pull()
cat(sprintf("weights g=%.2f l=%.2f y=%.2f | %.0fs | neighborhoods %d (planted 200) | purity %.2f | kept together %.2f | unclaimed %.1f%%\n",
  w_genre, w_lang, w_year, t1, dplyr::n_distinct(fn$neighborhood_id), purity, together, 100*mean(city$film_neighborhoods$from_noise)))
