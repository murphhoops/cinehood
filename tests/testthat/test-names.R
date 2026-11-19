# Names follow their films across reruns, whatever the new numbering.

names_cfg <- function(dir) {
  cfg <- cinehood_config()
  cfg$city_dir <- file.path(dir, "city")
  cfg$names_dir <- file.path(dir, "names")
  cfg
}

fake_run <- function(groups, districts) {
  dplyr::tibble(tmdb_id = seq_along(groups), neighborhood_id = groups, district_id = districts,
                placement = "core")
}

write_sheets <- function(fn, cfg, hood_names, district_names = NULL) {
  write_table(fn, "film_neighborhoods", cfg$city_dir)
  h <- dplyr::count(fn, neighborhood_id, name = "size")
  h$your_name <- hood_names[as.character(h$neighborhood_id)]
  utils::write.csv(h, file.path(cfg$city_dir, "neighborhood_names.csv"), row.names = FALSE, na = "")
  if (!is.null(district_names)) {
    d <- dplyr::count(fn, district_id, name = "size")
    d$your_name <- district_names[as.character(d$district_id)]
    utils::write.csv(d, file.path(cfg$city_dir, "district_names.csv"), row.names = FALSE, na = "")
  }
}

test_that("names carry over to renumbered neighborhoods and districts", {
  cfg <- names_cfg(withr::local_tempdir())
  run1 <- fake_run(rep(1:3, each = 20), rep(c(1, 1, 2), each = 20))
  write_sheets(run1, cfg, c("1" = "Shark Harbor", "2" = "Mob Row", "3" = "(no name yet)"),
               c("1" = "The Wilds", "2" = "The Underworld"))
  save_names(cfg)
  store <- load_names(cfg)
  expect_equal(sort(store$names$name), sort(c("Shark Harbor", "Mob Row", "The Wilds", "The Underworld")))

  # rerun: numbers shuffled, two films wander off
  run2 <- fake_run(rep(c(7, 4, 9), each = 20), rep(c(2, 2, 1), each = 20))
  run2$neighborhood_id[c(1, 2)] <- 9
  tabs <- list(neighborhoods = dplyr::tibble(neighborhood_id = c(4, 7, 9), draft_name = "x"),
               districts = dplyr::tibble(district_id = c(1, 2), draft_name = "y"))
  out <- apply_names(list(film_neighborhoods = run2), tabs, cfg)
  h <- out$neighborhoods
  expect_equal(h$your_name[h$neighborhood_id == 7], "Shark Harbor")
  expect_equal(h$your_name[h$neighborhood_id == 4], "Mob Row")
  expect_equal(h$your_name[h$neighborhood_id == 9], "")
  expect_equal(out$districts$your_name[out$districts$district_id == 2], "The Wilds")
  expect_equal(nrow(out$names_unmatched), 0)
})

test_that("when two named neighborhoods merge, one name keeps it and the other is reported", {
  cfg <- names_cfg(withr::local_tempdir())
  run1 <- fake_run(c(rep(1, 30), rep(2, 10)), rep(1, 40))
  write_sheets(run1, cfg, c("1" = "Hero Heights", "2" = "Hero Heights II"))
  save_names(cfg)
  run2 <- fake_run(rep(5, 40), rep(1, 40))
  tabs <- list(neighborhoods = dplyr::tibble(neighborhood_id = 5, draft_name = "x"),
               districts = dplyr::tibble(district_id = 1, draft_name = "y"))
  out <- apply_names(list(film_neighborhoods = run2), tabs, cfg)
  expect_equal(out$neighborhoods$your_name, "Hero Heights")
  expect_equal(out$names_unmatched$name, "Hero Heights II")
  expect_equal(out$names_unmatched$best_group, 5)
})

test_that("a neighborhood that breaks up loses its name", {
  cfg <- names_cfg(withr::local_tempdir())
  write_sheets(fake_run(rep(1, 30), rep(1, 30)), cfg, c("1" = "Grab Bag"))
  save_names(cfg)
  run2 <- fake_run(rep(1:3, each = 10), rep(1, 30))
  tabs <- list(neighborhoods = dplyr::tibble(neighborhood_id = 1:3, draft_name = "x"),
               districts = dplyr::tibble(district_id = 1, draft_name = "y"))
  out <- apply_names(list(film_neighborhoods = run2), tabs, cfg)
  expect_true(all(out$neighborhoods$your_name == ""))
  expect_equal(out$names_unmatched$name, "Grab Bag")
})

test_that("saving again updates names, keeps ids, and a cleared name is deleted", {
  cfg <- names_cfg(withr::local_tempdir())
  run1 <- fake_run(rep(1:2, each = 20), rep(1, 40))
  write_sheets(run1, cfg, c("1" = "Shark Harbor", "2" = "Mob Row"))
  save_names(cfg)
  ids <- load_names(cfg)$names
  sheet <- read_sheet(file.path(cfg$city_dir, "neighborhood_names.csv"))
  sheet$name_id <- ids$name_id[match(sheet$your_name, ids$name)]
  sheet$your_name <- c("Shark Bay", "")
  sheet$your_subtitle <- c("Killer animals", "")
  utils::write.csv(sheet, file.path(cfg$city_dir, "neighborhood_names.csv"), row.names = FALSE, na = "")
  save_names(cfg)
  after <- load_names(cfg)$names
  expect_equal(after$name, "Shark Bay")
  expect_equal(after$subtitle, "Killer animals")
  expect_equal(after$name_id, ids$name_id[ids$name == "Shark Harbor"])
})

test_that("a sheet from a different run is refused", {
  cfg <- names_cfg(withr::local_tempdir())
  write_sheets(fake_run(rep(1:2, each = 20), rep(1, 40)), cfg, c("1" = "A", "2" = "B"))
  write_table(fake_run(rep(1:2, c(25, 15)), rep(1, 40)), "film_neighborhoods", cfg$city_dir)
  expect_error(save_names(cfg), "doesn't match the latest run")
})

test_that("mood tags missed by the ID block count as tone", {
  expect_equal(classify_keyword(c("didactic", "taunting", "affectation", "shark"), c(1L, 2L, 3L, 4L)),
               c("tone", "tone", "tone", "content"))
})

test_that("when a small named group folds into a big one, the big one's name wins", {
  cfg <- names_cfg(withr::local_tempdir())
  # district 1 = 100 films, district 2 = 20 films
  run1 <- fake_run(rep(1, 120), c(rep(1, 100), rep(2, 20)))
  write_sheets(run1, cfg, c("1" = "x"), c("1" = "The Underworld", "2" = "The Shadows"))
  save_names(cfg)
  # rerun: all of district 2 and 90 of district 1 now share district 3
  run2 <- fake_run(rep(1, 120), c(rep(3, 90), rep(4, 10), rep(3, 20)))
  tabs <- list(neighborhoods = dplyr::tibble(neighborhood_id = 1, draft_name = "x"),
               districts = dplyr::tibble(district_id = c(3, 4), draft_name = "y"))
  out <- apply_names(list(film_neighborhoods = run2), tabs, cfg)
  expect_equal(out$districts$your_name[out$districts$district_id == 3], "The Underworld")
  expect_equal(out$names_unmatched$name, "The Shadows")
})
