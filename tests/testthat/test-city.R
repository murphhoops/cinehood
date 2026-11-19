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
  cfg$names_dir <- file.path(dir, "names")
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

test_that("names typed into the sheets show up in the next report", {
  f <- city_fixture()
  write_city(f$city, f$tabs, f$cfg)
  sheet_path <- file.path(f$cfg$city_dir, "neighborhood_names.csv")
  sheet <- read_sheet(sheet_path)
  sheet$your_name <- ""
  sheet$your_name[1] <- "Test Town"
  utils::write.csv(sheet, sheet_path, row.names = FALSE, na = "")
  save_names(f$cfg)
  tabs <- apply_names(f$city, f$tabs, f$cfg)
  expect_equal(tabs$neighborhoods$your_name[1], "Test Town")
  write_city(f$city, tabs, f$cfg)
  expect_equal(read_sheet(sheet_path)$your_name[1], "Test Town")
  rep <- readLines(city_report(f$city, tabs, f$cfg))
  expect_true(any(grepl("Test Town (", rep, fixed = TRUE)))
  expect_true(any(grepl("1 of", rep, fixed = TRUE)))
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

test_that("a neighborhood straddling a hard genre line is split in two", {
  cfg <- city_test_cfg(withr::local_tempdir())
  ids <- 1:60
  label <- rep(1L, 60)
  genres <- dplyr::tibble(tmdb_id = ids, genre = c(rep("Horror", 25), rep("Drama", 35)))
  out <- split_hard_genres(label, ids, genres, cfg)
  expect_equal(out$splits, 1L)
  expect_equal(length(unique(out$label[1:25])), 1)
  expect_false(out$label[1] == out$label[60])
  # a few horror films in a drama neighborhood: not a real mix, left alone
  genres2 <- dplyr::tibble(tmdb_id = ids, genre = c(rep("Horror", 8), rep("Drama", 52)))
  expect_equal(split_hard_genres(label, ids, genres2, cfg)$splits, 0L)
})

test_that("death-themed horror and grief dramas end up in different neighborhoods", {
  set.seed(3)
  n <- 80
  films <- dplyr::tibble(tmdb_id = 1:n, title = paste("Film", 1:n), year = 2005L,
                         original_language = "en", runtime = 105L, vote_count = 1000L)
  pool <- paste0("death_kw", 1:40)
  kw <- dplyr::bind_rows(lapply(1:n, function(i) dplyr::tibble(tmdb_id = i, keyword = sample(pool, 5))))
  # 40 other films to give the city more than one place to be
  other <- dplyr::tibble(tmdb_id = 81:120, title = paste("Other", 1:40), year = 1990L,
                         original_language = "en", runtime = 100L, vote_count = 500L)
  kw <- dplyr::bind_rows(kw, dplyr::bind_rows(lapply(81:120, function(i)
    dplyr::tibble(tmdb_id = i, keyword = sample(paste0("west_kw", 1:40), 5)))))
  kw$keyword_id <- match(kw$keyword, unique(kw$keyword))
  genres <- dplyr::tibble(tmdb_id = 1:120, genre = c(rep("Horror", 40), rep("Drama", 40), rep("Western", 40)))
  spine <- list(films = dplyr::bind_rows(films, other), film_keywords = kw, film_genres = genres)
  cfg <- city_test_cfg(withr::local_tempdir())
  cfg$city_districts <- 2
  fn <- build_city(spine, cfg)$film_neighborhoods
  horror_hoods <- unique(fn$neighborhood_id[fn$tmdb_id <= 40])
  drama_hoods <- unique(fn$neighborhood_id[fn$tmdb_id > 40 & fn$tmdb_id <= 80])
  expect_length(intersect(horror_hoods, drama_hoods), 0)
})

test_that("every film gets a fit score, and examples come from the heart of a neighborhood", {
  f <- city_fixture()
  fn <- f$city$film_neighborhoods
  expect_true(all(fn$fit > 0 & fn$fit <= 1))
  # core films that fit firmly beat a famous edge film for the examples
  o <- example_rank(c("nearest", "core", "core", "loose fit"), c(0.9, 0.7, 0.55, 0.9), c(100000, 50, 9000, 80000))
  expect_equal(o, c(2, 3, 1, 4))
  expect_true(all(c("core_share", "mean_fit") %in% names(f$tabs$neighborhoods)))
})

test_that("the report lists neighborhoods worth a second look", {
  f <- city_fixture()
  rep <- readLines(city_report(f$city, f$tabs, f$cfg))
  expect_true(any(grepl("^## Worth a second look", rep)))
  expect_true(any(grepl("split on a hard genre line", rep)))
})

test_that("plot events like murder and grief count as generic", {
  expect_equal(classify_keyword(c("murder", "loss of loved one", "death of father", "kidnapping", "kaiju"),
                                c(9826L, 697L, 10201L, 1930L, 161791L)),
               c("light", "light", "light", "light", "content"))
})

test_that("a few films across a hard line move to their most similar films on their side", {
  ids <- 1:100
  # neighborhood 1: 57 ocean adventures + 3 documentaries; 2: 20 documentaries; 3: 20 desert adventures
  label <- c(rep(1L, 60), rep(2L, 20), rep(3L, 20))
  genres <- dplyr::tibble(tmdb_id = ids,
                          genre = c(rep("Adventure", 57), rep("Documentary", 3), rep("Documentary", 20), rep("Adventure", 20)))
  side <- hard_sides(ids, genres, c("Horror", "Animation", "Documentary"))
  set.seed(3)
  Z <- matrix(abs(rnorm(100 * 6, sd = 0.05)), 100)
  Z[1:60, 1] <- Z[1:60, 1] + 1                 # ocean
  Z[58:60, 2] <- Z[58:60, 2] + 1.2             # the 3 ocean documentaries lean documentary
  Z[61:80, 2] <- Z[61:80, 2] + 1               # documentaries
  Z[81:100, 3] <- Z[81:100, 3] + 1             # desert
  out <- enforce_hard_lines(label, side, Z, k = 10)
  expect_equal(out$label[58:60], c(2L, 2L, 2L))      # the documentaries moved
  expect_equal(sum(out$moved), 3)
  expect_true(all(out$label[1:57] == 1L))            # the adventures stayed
  # with no documentary neighborhood to go to, they stay put
  keep <- c(1:60, 81:100)
  out2 <- enforce_hard_lines(label[keep], side[keep, , drop = FALSE], Z[keep, ], k = 10)
  expect_equal(sum(out2$moved), 0)
})

test_that("placed films join their most similar films, and respect hard lines", {
  ids <- 1:60
  genres <- dplyr::tibble(tmdb_id = ids, genre = c(rep("Drama", 20), rep("Horror", 20), rep("Drama", 20)))
  side <- hard_sides(ids, genres, "Horror")
  Z <- rbind(cbind(1, 0, matrix(0.01, 20, 2)), cbind(0, 1, matrix(0.01, 20, 2)), cbind(0.9, 0.1, matrix(0.01, 20, 2)))
  core <- 1:40; core_label <- c(rep(1L, 20), rep(2L, 20))
  # drama films 41:60 sit near hood 1
  expect_true(all(place_by_neighbors(Z, 41:60, core, core_label, side, k = 5) == 1L))
  # a horror film that looks like the dramas still goes to the horror neighborhood
  side2 <- side; side2[41, 1] <- TRUE
  expect_equal(place_by_neighbors(Z, 41L, core, core_label, side2, k = 5), 2L)
})

test_that("a big neighborhood with two clear groups inside is re-split", {
  set.seed(1)
  a <- matrix(rnorm(150 * 20, sd = 0.05), 150) ; a[, 1:10] <- a[, 1:10] + 1
  b <- matrix(rnorm(150 * 20, sd = 0.05), 150) ; b[, 11:20] <- b[, 11:20] + 1
  Z <- rbind(a, b)
  cfg <- cinehood_config(); cfg$city_max_core <- 200
  out <- split_big_neighborhoods(rep(1L, 300), Z, cfg)
  expect_equal(out$resplits, 1L)
  expect_equal(length(unique(out$label)), 2)
  expect_true(all(out$label[1:150] == out$label[1]))
  expect_true(all(out$label[151:300] == out$label[151]))

  small <- split_big_neighborhoods(rep(1L, 300), Z, modifyList(cfg, list(city_max_core = 400)))
  expect_equal(small$resplits, 0L)
})

test_that("hand-picked districts: yours, then by name, then by genre, then nearest", {
  f <- city_fixture()
  cfg <- f$cfg
  cfg$names_dir <- withr::local_tempdir()
  utils::write.csv(data.frame(name = c("The Underworld", "The Haunts", "Old Town"),
                              subtitle = c("Crime", "Spooky", "Classics"),
                              claims_genre = c("", "Horror", "")),
                   file.path(cfg$names_dir, "districts.csv"), row.names = FALSE)
  utils::write.csv(data.frame(neighborhood = "Heist Row", district = "The Underworld"),
                   file.path(cfg$names_dir, "district_defaults.csv"), row.names = FALSE)
  joined <- dplyr::inner_join(f$city$film_neighborhoods, f$spine$films, by = "tmdb_id")
  major <- joined |> dplyr::count(neighborhood_id, true_group) |>
    dplyr::group_by(neighborhood_id) |> dplyr::slice_max(n, n = 1, with_ties = FALSE)
  hood_of <- function(g) major$neighborhood_id[major$true_group == g][1]
  tabs <- f$tabs
  tabs$neighborhoods$your_name <- ifelse(tabs$neighborhoods$neighborhood_id == hood_of("heist"), "Heist Row", "")
  tabs$neighborhoods$your_district <- ifelse(tabs$neighborhoods$neighborhood_id == hood_of("samurai"),
                                             "Old Town", "")
  out <- assign_districts(f$city, tabs, f$spine, cfg)
  h <- out$tabs$neighborhoods
  at <- function(g) h[h$neighborhood_id == hood_of(g), ]
  expect_equal(at("samurai")$your_district, "Old Town");       expect_equal(at("samurai")$district_source, "you")
  expect_equal(at("heist")$your_district, "The Underworld");   expect_equal(at("heist")$district_source, "default")
  expect_equal(at("ghosts")$your_district, "The Haunts");      expect_equal(at("ghosts")$district_source, "genre")
  expect_equal(at("boxing")$district_source, "nearest")
  expect_true(all(h$your_district %in% c("The Underworld", "The Haunts", "Old Town")))
  # films follow their neighborhood's district, and districts are numbered in list order
  fn <- out$city$film_neighborhoods
  expect_equal(fn$district_id, h$district_id[match(fn$neighborhood_id, h$neighborhood_id)])
  expect_equal(at("samurai")$district_id, 3)
  expect_true(all(out$tabs$districts$your_name %in% c("The Underworld", "The Haunts", "Old Town")))
  # the sheet carries the district and no district sheet is written
  write_city(out$city, out$tabs, modifyList(cfg, list(city_dir = withr::local_tempdir())))
  rep <- readLines(city_report(out$city, out$tabs, cfg))
  expect_true(any(grepl("hand-picked list of 3", rep, fixed = TRUE)))
})

test_that("without a district list, districts come from the data", {
  f <- city_fixture()
  cfg <- f$cfg
  cfg$names_dir <- withr::local_tempdir()
  out <- assign_districts(f$city, f$tabs, f$spine, cfg)
  expect_identical(out$city, f$city)
})
