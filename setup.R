# Packages for the pipeline. Run once (and again after a new phase): source("setup.R")
#
# Installs one package at a time, with retries. Some networks cut connections
# when RStudio downloads several packages at once; one at a time gets through.

pkgs <- c("httr2", "jsonlite", "dplyr", "tidyr", "purrr", "stringr",
          "arrow", "targets", "visNetwork", "testthat", "withr", "usethis",
          # phase 2: the city
          "Matrix", "irlba", "uwot", "dbscan", "ggplot2")

options(repos = c(CRAN = "https://cloud.r-project.org"),
        install.packages.compile.from.source = "never")

install_one_by_one <- function(pkgs, tries = 4) {
  deps <- tools::package_dependencies(pkgs, recursive = TRUE,
                                      which = c("Depends", "Imports", "LinkingTo"))
  todo <- unique(c(rev(unlist(deps)), pkgs))
  todo <- setdiff(todo, c(rownames(utils::installed.packages()), "R"))
  missing <- character()
  for (p in todo) {
    for (k in seq_len(tries)) {
      try(utils::install.packages(p, dependencies = FALSE, quiet = TRUE), silent = TRUE)
      if (requireNamespace(p, quietly = TRUE)) break
      Sys.sleep(3)
    }
    ok <- requireNamespace(p, quietly = TRUE)
    message(p, if (ok) ": installed" else ": STILL MISSING")
    if (!ok) missing <- c(missing, p)
  }
  missing
}

missing <- install_one_by_one(pkgs)
if (length(missing)) {
  message("Couldn't install: ", paste(missing, collapse = ", "),
          ". Run source(\"setup.R\") again; it only retries what's missing.")
} else {
  message("ready")
}
