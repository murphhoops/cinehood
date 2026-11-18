# Packages for the pipeline. Run once (and again after a new phase): source("setup.R")
pkgs <- c("httr2", "jsonlite", "dplyr", "tidyr", "purrr", "stringr",
          "arrow", "targets", "visNetwork", "testthat", "withr", "usethis",
          # phase 2: the city
          "Matrix", "irlba", "uwot", "dbscan", "ggplot2")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
message("ready")
