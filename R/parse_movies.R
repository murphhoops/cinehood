# Step 3: raw TMDB JSON -> tidy tables, all keyed on tmdb_id.
#   films           one row per film
#   film_genres     film x genre
#   film_people     film x person (directors, writers, top-billed cast)
#   film_keywords   film x keyword
#   film_companies  film x production company
#   film_countries  film x production country

`%||%` <- function(x, y) if (is.null(x)) y else x
chr <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x)
num <- function(x) if (is.null(x) || !length(x)) NA_real_ else as.numeric(x)
int <- function(x) if (is.null(x) || !length(x)) NA_integer_ else as.integer(x)

# US certification (PG-13, R, ...) from the theatrical release, if TMDB has one.
us_certification <- function(m) {
  for (country in m$release_dates$results %||% list()) {
    if (identical(country$iso_3166_1, "US")) {
      certs <- vapply(country$release_dates %||% list(),
                      function(r) chr(r$certification), character(1))
      certs <- certs[!is.na(certs) & nzchar(certs)]
      if (length(certs)) return(certs[[1]])
    }
  }
  NA_character_
}

parse_film_row <- function(m) {
  rd <- chr(m$release_date)
  dplyr::tibble(
    tmdb_id           = int(m$id),
    imdb_id           = chr(m$imdb_id),
    title             = chr(m$title),
    original_title    = chr(m$original_title),
    original_language = chr(m$original_language),
    release_date      = if (is.na(rd) || !nzchar(rd)) as.Date(NA) else as.Date(rd),
    status            = chr(m$status),
    runtime           = int(m$runtime),
    overview          = chr(m$overview),
    tagline           = chr(m$tagline),
    poster_path       = chr(m$poster_path),
    popularity        = num(m$popularity),
    vote_average      = num(m$vote_average),
    vote_count        = int(m$vote_count),
    budget            = num(m$budget),
    revenue           = num(m$revenue),
    collection_id     = int(m$belongs_to_collection$id),
    collection_name   = chr(m$belongs_to_collection$name),
    us_certification  = us_certification(m),
    fetched_at        = as.Date(chr(m$.fetched_at))
  )
}

# A list of objects -> a tibble with the chosen fields, plus the film's id.
pluck_rows <- function(m, items, fields) {
  items <- items %||% list()
  if (!length(items)) return(NULL)
  cols <- lapply(fields, function(f) {
    vals <- lapply(items, function(x) x[[f]])
    if (all(vapply(vals, function(v) is.null(v) || is.numeric(v), logical(1)))) {
      vapply(vals, num, numeric(1))
    } else {
      vapply(vals, chr, character(1))
    }
  })
  names(cols) <- fields
  dplyr::bind_cols(dplyr::tibble(tmdb_id = int(m$id)), dplyr::as_tibble(cols))
}

parse_people <- function(m, cast_n) {
  crew <- pluck_rows(m, m$credits$crew, c("id", "name", "department", "job"))
  cast <- pluck_rows(m, m$credits$cast, c("id", "name", "character", "order"))

  directors <- if (!is.null(crew)) {
    crew |> dplyr::filter(job == "Director") |>
      dplyr::transmute(tmdb_id, person_id = as.integer(id), name, role = "director",
                       billing = NA_integer_, detail = job)
  }
  writers <- if (!is.null(crew)) {
    crew |> dplyr::filter(department == "Writing") |>
      dplyr::distinct(id, .keep_all = TRUE) |>
      dplyr::transmute(tmdb_id, person_id = as.integer(id), name, role = "writer",
                       billing = NA_integer_, detail = job)
  }
  actors <- if (!is.null(cast)) {
    cast |> dplyr::arrange(order) |> dplyr::slice_head(n = cast_n) |>
      dplyr::transmute(tmdb_id, person_id = as.integer(id), name, role = "cast",
                       billing = as.integer(order) + 1L, detail = character)
  }
  dplyr::bind_rows(directors, writers, actors)
}

# Stack one sub-list per film; if no film had any, return the empty template so
# every table keeps its columns and types.
stack <- function(movies, f, shape, template) {
  df <- dplyr::bind_rows(lapply(movies, f))
  if (!nrow(df)) return(template)
  shape(df)
}

parse_movies <- function(movies, cast_n = 5) {
  list(
    films = dplyr::bind_rows(lapply(movies, parse_film_row)),
    film_genres = stack(movies,
      function(m) pluck_rows(m, m$genres, c("id", "name")),
      function(d) dplyr::transmute(d, tmdb_id, genre_id = as.integer(id), genre = name),
      dplyr::tibble(tmdb_id = integer(), genre_id = integer(), genre = character())),
    film_people = stack(movies,
      function(m) parse_people(m, cast_n),
      identity,
      dplyr::tibble(tmdb_id = integer(), person_id = integer(), name = character(),
                    role = character(), billing = integer(), detail = character())),
    film_keywords = stack(movies,
      function(m) pluck_rows(m, m$keywords$keywords, c("id", "name")),
      function(d) dplyr::transmute(d, tmdb_id, keyword_id = as.integer(id), keyword = name),
      dplyr::tibble(tmdb_id = integer(), keyword_id = integer(), keyword = character())),
    film_companies = stack(movies,
      function(m) pluck_rows(m, m$production_companies, c("id", "name", "origin_country")),
      function(d) dplyr::transmute(d, tmdb_id, company_id = as.integer(id), company = name, origin_country),
      dplyr::tibble(tmdb_id = integer(), company_id = integer(), company = character(),
                    origin_country = character())),
    film_countries = stack(movies,
      function(m) pluck_rows(m, m$production_countries, c("iso_3166_1", "name")),
      function(d) dplyr::transmute(d, tmdb_id, country_code = iso_3166_1, country = name),
      dplyr::tibble(tmdb_id = integer(), country_code = character(), country = character()))
  )
}
