# Packages for phase 1. Run once: source("setup.R")
pkgs <- c("httr2", "jsonlite", "dplyr", "tidyr", "purrr", "stringr",
          "arrow", "targets", "visNetwork", "testthat", "withr", "usethis")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
message("ready")
