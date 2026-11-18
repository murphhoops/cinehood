# Talking to the TMDB API.
# Auth: put your v4 "API Read Access Token" in .Renviron as TMDB_TOKEN=...
# (usethis::edit_r_environ(), then restart R). Never commit it.

tmdb_token <- function() {
  tok <- Sys.getenv("TMDB_TOKEN")
  if (!nzchar(tok)) {
    stop("TMDB_TOKEN is not set. Add your TMDB API Read Access Token to ",
         ".Renviron (usethis::edit_r_environ()) and restart R.", call. = FALSE)
  }
  tok
}

# One movie's details, with credits, keywords and release dates folded in so a
# single request gets everything phase 1 needs.
tmdb_movie_request <- function(id, token = tmdb_token(), language = "en-US") {
  httr2::request("https://api.themoviedb.org/3") |>
    httr2::req_url_path_append("movie", id) |>
    httr2::req_url_query(
      append_to_response = "credits,keywords,release_dates",
      language = language
    ) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_user_agent("cinehood-pipeline (github.com/murphhoops/cinehood)") |>
    httr2::req_timeout(30) |>
    # Hand every HTTP status back as a response, so a 404 (deleted movie) is a
    # result to record rather than an error that stops the run.
    httr2::req_error(is_error = function(resp) FALSE)
}

# Send a request, and if the connection itself fails (reset, timeout, DNS
# blip), wait and try again. HTTP errors such as 429 are handled separately by
# each request's req_retry(); this covers the network dropping out underneath.
perform_with_retry <- function(req, tries = 5, waits = c(2, 5, 10, 20),
                               perform = httr2::req_perform, sleep = Sys.sleep) {
  for (k in seq_len(tries)) {
    res <- tryCatch(perform(req), error = function(e) e)
    if (!inherits(res, "error")) return(res)
    if (k < tries) {
      wait <- waits[min(k, length(waits))]
      message("connection problem (", conditionMessage(res), "); retrying in ", wait, "s")
      sleep(wait)
    }
  }
  stop(res)
}
