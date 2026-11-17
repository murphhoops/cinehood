# Writing and reading the spine's tables.
# Parquet when {arrow} is installed (the target format); .rds otherwise.

table_path <- function(name, dir) {
  ext <- if (requireNamespace("arrow", quietly = TRUE)) "parquet" else "rds"
  file.path(dir, paste0(name, ".", ext))
}

write_table <- function(df, name, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- table_path(name, dir)
  if (grepl("\\.parquet$", path)) {
    arrow::write_parquet(df, path)
  } else {
    saveRDS(df, path)
  }
  path
}

read_table <- function(name, dir) {
  pq  <- file.path(dir, paste0(name, ".parquet"))
  rds <- file.path(dir, paste0(name, ".rds"))
  if (file.exists(pq) && requireNamespace("arrow", quietly = TRUE)) {
    return(dplyr::as_tibble(arrow::read_parquet(pq)))
  }
  if (file.exists(rds)) return(readRDS(rds))
  stop("No table called '", name, "' in ", dir, call. = FALSE)
}
