# A made-up catalog with five known kinds of film, to check that the city
# finds them. Invented data, not real films.
fake_city_spine <- function(per_group = 80, seed = 7) {
  set.seed(seed)
  groups <- list(
    heist   = list(genre = "Crime",   year = 2005, lang = "en"),
    boxing  = list(genre = "Drama",   year = 1985, lang = "en"),
    ghosts  = list(genre = "Horror",  year = 2015, lang = "en"),
    samurai = list(genre = "Action",  year = 1962, lang = "ja"),
    wedding = list(genre = "Romance", year = 1998, lang = "en")
  )
  shared_pool <- paste0("common", 1:40)
  rows <- list(); kws <- list(); gens <- list(); id <- 1L
  for (g in names(groups)) {
    pool <- paste0(g, "_kw", 1:12)
    for (k in seq_len(per_group)) {
      yr <- as.integer(round(stats::rnorm(1, groups[[g]]$year, 4)))
      rows[[length(rows) + 1]] <- dplyr::tibble(
        tmdb_id = id, title = paste(g, k), year = yr, original_language = groups[[g]]$lang,
        runtime = as.integer(round(stats::rnorm(1, 110, 12))), vote_count = sample(300:5000, 1),
        true_group = g)
      chosen <- c(sample(pool, 5), sample(shared_pool, 2))
      kws[[length(kws) + 1]] <- dplyr::tibble(tmdb_id = id, keyword = chosen)
      gens[[length(gens) + 1]] <- dplyr::tibble(
        tmdb_id = id, genre = unique(c(groups[[g]]$genre, sample(c("Comedy", "Thriller", "Drama"), 1))))
      id <- id + 1L
    }
  }
  kw <- dplyr::bind_rows(kws)
  kw$keyword_id <- match(kw$keyword, sort(unique(kw$keyword)))
  list(films = dplyr::bind_rows(rows), film_keywords = kw, film_genres = dplyr::bind_rows(gens))
}

city_test_cfg <- function(dir) {
  cfg <- test_cfg(dir)
  cfg$city_dir <- file.path(dir, "city")
  cfg$city_neighbors <- 15
  cfg$city_districts <- 3
  cfg$city_review_n <- 5
  cfg
}

# Built once, shared by the tests below (UMAP is the slow part).
city_fixture <- local({
  built <- NULL
  function() {
    if (is.null(built)) {
      spine <- fake_city_spine()
      cfg <- city_test_cfg(withr::local_tempdir(.local_envir = globalenv()))
      city <- build_city(spine, cfg)
      built <<- list(spine = spine, cfg = cfg, city = city, tabs = city_tables(city, spine, cfg))
    }
    built
  }
})

test_that("directors are not features, and stoplisted keywords are left out", {
  spine <- fake_city_spine(per_group = 20)
  spine$film_keywords <- dplyr::bind_rows(spine$film_keywords,
    dplyr::tibble(tmdb_id = 1:20, keyword = "woman director", keyword_id = 9999L))
  cfg <- city_test_cfg(withr::local_tempdir())
  vocab <- keyword_vocab(spine$film_keywords, nrow(spine$films), cfg)
  expect_false("woman director" %in% vocab$keyword)
  feat <- city_features(spine, cfg)
  expect_false(any(grepl("director", c(colnames(feat$X_kw), colnames(feat$side)), ignore.case = TRUE)))
})

test_that("every film lands in exactly one neighborhood and district", {
  f <- city_fixture()
  fn <- f$city$film_neighborhoods
  expect_equal(nrow(fn), 400)
  expect_false(anyNA(fn$neighborhood_id))
  expect_true(all(fn$neighborhood_id >= 1))
  expect_true(all(fn$district_id %in% 1:3))
  # one district per neighborhood
  expect_equal(nrow(dplyr::distinct(fn, neighborhood_id, district_id)), dplyr::n_distinct(fn$neighborhood_id))
})

test_that("neighborhoods recover the known kinds of film", {
  f <- city_fixture()
  joined <- dplyr::inner_join(f$city$film_neighborhoods, f$spine$films, by = "tmdb_id")
  purity <- joined |>
    dplyr::count(neighborhood_id, true_group) |>
    dplyr::group_by(neighborhood_id) |>
    dplyr::summarise(top = max(n), size = sum(n)) |>
    dplyr::summarise(p = sum(top) / sum(size)) |>
    dplyr::pull(p)
  expect_gt(purity, 0.9)
  # and each kind mostly lives together rather than being scattered
  spread <- joined |> dplyr::count(true_group, neighborhood_id) |>
    dplyr::group_by(true_group) |> dplyr::summarise(main_share = max(n) / sum(n))
  expect_true(all(spread$main_share > 0.5))
})

test_that("neighborhoods are numbered largest first", {
  f <- city_fixture()
  sizes <- dplyr::count(f$city$film_neighborhoods, neighborhood_id)$n
  expect_equal(sizes, sort(sizes, decreasing = TRUE))
})

test_that("draft names come from each neighborhood's own keywords", {
  f <- city_fixture()
  joined <- dplyr::inner_join(f$city$film_neighborhoods, f$spine$films, by = "tmdb_id")
  majority <- joined |> dplyr::count(neighborhood_id, true_group) |>
    dplyr::group_by(neighborhood_id) |> dplyr::slice_max(n, n = 1, with_ties = FALSE)
  h <- dplyr::inner_join(f$tabs$neighborhoods, majority, by = "neighborhood_id")
  first_word <- vapply(strsplit(h$draft_name, " / ", fixed = TRUE), `[`, character(1), 1)
  expect_true(all(startsWith(first_word, paste0(h$true_group, "_kw"))))
  expect_false(any(grepl("common", first_word)))
})

test_that("tables, naming sheet, map and report are written", {
  f <- city_fixture()
  paths <- write_city(f$city, f$tabs, f$cfg)
  expect_true(all(file.exists(paths)))
  sheet <- utils::read.csv(file.path(f$cfg$city_dir, "neighborhood_names.csv"))
  expect_true("your_name" %in% names(sheet))
  expect_equal(nrow(sheet), nrow(f$tabs$neighborhoods))
  expect_equal(nrow(read_table("film_neighborhoods", f$cfg$city_dir)), 400)
  map <- city_map(f$city, f$tabs, f$cfg)
  expect_true(file.exists(map))
  rep <- readLines(city_report(f$city, f$tabs, f$cfg))
  expect_true(any(grepl("^\\| Neighborhoods \\|", rep)))
  expect_true(any(grepl("random neighborhoods, for a check by eye", rep)))
})

test_that("the same settings give the same city", {
  spine <- fake_city_spine(per_group = 40)
  cfg <- city_test_cfg(withr::local_tempdir())
  a <- build_city(spine, cfg)$film_neighborhoods
  b <- build_city(spine, cfg)$film_neighborhoods
  expect_equal(a$neighborhood_id, b$neighborhood_id)
})

test_that("keywords are sorted into tone, production, light and content", {
  k <- dplyr::tribble(
    ~keyword, ~keyword_id, ~kind,
    "admiring", 325761L, "tone",          # TMDB's mood-tag block
    "wry", 325857L, "tone",
    "suspenseful", 314730L, "tone",       # older mood word
    "black and white", 12999L, "production",
    "sequel", 9663L, "production",
    "father daughter relationship", 240303L, "light",
    "los angeles, california", 12670L, "light",
    "1970s", 1228L, "light",
    "19th century", 207928L, "light",
    "new york city", 242L, "light",
    "heist", 10051L, "content",
    "kaiju", 161791L, "content",
    "film noir", 9807L, "content"
  )
  expect_equal(classify_keyword(k$keyword, k$keyword_id), k$kind)
})

test_that("mood and production keywords stay out of the map; generic ones count for less", {
  spine <- fake_city_spine(per_group = 20)
  extra <- dplyr::bind_rows(
    dplyr::tibble(tmdb_id = 1:60, keyword = "admiring", keyword_id = 325761L),
    dplyr::tibble(tmdb_id = 1:60, keyword = "black and white", keyword_id = 12999L),
    dplyr::tibble(tmdb_id = 1:8, keyword = "father son relationship", keyword_id = 240119L))
  spine$film_keywords <- dplyr::bind_rows(spine$film_keywords, extra)
  cfg <- city_test_cfg(withr::local_tempdir())
  vocab <- keyword_vocab(spine$film_keywords, nrow(spine$films), cfg)
  expect_false(any(c("admiring", "black and white") %in% vocab$keyword))
  expect_equal(vocab$weight[vocab$keyword == "father son relationship"], 0.5)
  expect_true(all(vocab$weight[vocab$kind == "content"] == 1))
})

test_that("films with too few keywords are placed afterwards, near their own kind", {
  spine <- fake_city_spine()
  # 15 extra heist films that carry only 2 heist keywords each
  sparse_ids <- 1000L + 1:15
  spine$films <- dplyr::bind_rows(spine$films, dplyr::tibble(
    tmdb_id = sparse_ids, title = paste("thin heist", 1:15), year = 2005L,
    original_language = "en", runtime = 110L, vote_count = 500L, true_group = "heist"))
  thin_kw <- dplyr::tibble(tmdb_id = rep(sparse_ids, each = 2), keyword = rep(c("heist_kw1", "heist_kw2"), 15))
  thin_kw$keyword_id <- spine$film_keywords$keyword_id[match(thin_kw$keyword, spine$film_keywords$keyword)]
  spine$film_keywords <- dplyr::bind_rows(spine$film_keywords, thin_kw)
  spine$film_genres <- dplyr::bind_rows(spine$film_genres, dplyr::tibble(tmdb_id = sparse_ids, genre = "Crime"))

  cfg <- city_test_cfg(withr::local_tempdir())
  fn <- build_city(spine, cfg)$film_neighborhoods
  thin <- fn[fn$tmdb_id %in% sparse_ids, ]
  expect_true(all(thin$placement == "loose fit"))
  expect_true(all(fn$placement[!fn$tmdb_id %in% sparse_ids] %in% c("core", "nearest")))
  heist_home <- fn |> dplyr::filter(tmdb_id <= 400) |>
    dplyr::inner_join(spine$films, by = "tmdb_id") |> dplyr::filter(true_group == "heist") |>
    dplyr::count(neighborhood_id, sort = TRUE) |> dplyr::slice(1) |> dplyr::pull(neighborhood_id)
  expect_gt(mean(thin$neighborhood_id == heist_home), 0.8)
  expect_false(anyNA(thin$x))
})
