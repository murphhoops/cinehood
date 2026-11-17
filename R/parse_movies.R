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

# One value per film, typed: col(movies, function(m) m$runtime, int, integer(1)).
col <- function(movies, get, conv, proto) {
  vapply(movies, function(m) conv(get(m)), proto)
}

parse_films <- function(movies) {
  rd <- col(movies, function(m) m$release_date, chr, character(1))
  rd[!is.na(rd) & !nzchar(rd)] <- NA
  dplyr::tibble(
    tmdb_id           = col(movies, function(m) m$id, int, integer(1)),
    imdb_id           = col(movies, function(m) m$imdb_id, chr, character(1)),
    title             = col(movies, function(m) m$title, chr, character(1)),
    original_title    = col(movies, function(m) m$original_title, chr, character(1)),
    original_language = col(movies, function(m) m$original_language, chr, character(1)),
    release_date      = as.Date(rd),
    status            = col(movies, function(m) m$status, chr, character(1)),
    runtime           = col(movies, function(m) m$runtime, int, integer(1)),
    overview          = col(movies, function(m) m$overview, chr, character(1)),
    tagline           = col(movies, function(m) m$tagline, chr, character(1)),
    poster_path       = col(movies, function(m) m$poster_path, chr, character(1)),
    popularity        = col(movies, function(m) m$popularity, num, numeric(1)),
    vote_average      = col(movies, function(m) m$vote_average, num, numeric(1)),
    vote_count        = col(movies, function(m) m$vote_count, int, integer(1)),
    budget            = col(movies, function(m) m$budget, num, numeric(1)),
    revenue           = col(movies, function(m) m$revenue, num, numeric(1)),
    collection_id     = col(movies, function(m) m$belongs_to_collection$id, int, integer(1)),
    collection_name   = col(movies, function(m) m$belongs_to_collection$name, chr, character(1)),
    us_certification  = vapply(movies, us_certification, character(1)),
    fetched_at        = as.Date(col(movies, function(m) m$.fetched_at, chr, character(1)))
  )
}

# Every film's sub-list (its genres, keywords, ...) flattened into one table,
# one row per item, with the film's id. `fields` maps each field to a
# converter: list(id = int, name = chr). Builds each column in one pass instead
# of a small table per film, which is what kept the first version slow.
explode <- function(movies, get, fields) {
  items <- lapply(movies, function(m) get(m) %||% list())
  flat <- unlist(items, recursive = FALSE)
  out <- list(tmdb_id = rep(col(movies, function(m) m$id, int, integer(1)), lengths(items)))
  for (f in names(fields)) {
    conv <- fields[[f]]
    proto <- if (identical(conv, int)) integer(1) else if (identical(conv, num)) numeric(1) else character(1)
    out[[f]] <- vapply(flat, function(x) conv(x[[f]]), proto)
  }
  dplyr::as_tibble(out)
}

# Only the crew rows phase 1 keeps (directors and writers), and only the top
# billed cast: real films list 100+ crew and 40+ cast, most of which we drop.
crew_kept <- function(m) {
  Filter(function(c) identical(c$job, "Director") || identical(c$department, "Writing"),
         m$credits$crew %||% list())
}

cast_top <- function(cast_n) {
  function(m) {
    cs <- m$credits$cast %||% list()
    if (!length(cs)) return(cs)
    ord <- vapply(cs, function(c) num(c$order), numeric(1))
    cs[order(ord)][seq_len(min(cast_n, length(cs)))]
  }
}

parse_people <- function(movies, cast_n) {
  crew <- explode(movies, crew_kept, list(id = int, name = chr, department = chr, job = chr))
  cast <- explode(movies, cast_top(cast_n), list(id = int, name = chr, character = chr, order = int))

  directors <- crew |> dplyr::filter(job == "Director") |>
    dplyr::transmute(tmdb_id, person_id = id, name, role = "director",
                     billing = NA_integer_, detail = job)
  writers <- crew |> dplyr::filter(department == "Writing") |>
    dplyr::distinct(tmdb_id, id, .keep_all = TRUE) |>
    dplyr::transmute(tmdb_id, person_id = id, name, role = "writer",
                     billing = NA_integer_, detail = job)
  actors <- cast |>
    dplyr::transmute(tmdb_id, person_id = id, name, role = "cast",
                     billing = order + 1L, detail = character)

  film_order <- col(movies, function(m) m$id, int, integer(1))
  dplyr::bind_rows(directors, writers, actors) |>
    dplyr::arrange(match(tmdb_id, film_order),
                   match(role, c("director", "writer", "cast")), billing)
}

parse_movies <- function(movies, cast_n = 5) {
  list(
    films = parse_films(movies),
    film_genres = explode(movies, function(m) m$genres, list(id = int, name = chr)) |>
      dplyr::transmute(tmdb_id, genre_id = id, genre = name),
    film_people = parse_people(movies, cast_n),
    film_keywords = explode(movies, function(m) m$keywords$keywords, list(id = int, name = chr)) |>
      dplyr::transmute(tmdb_id, keyword_id = id, keyword = name),
    film_companies = explode(movies, function(m) m$production_companies,
                             list(id = int, name = chr, origin_country = chr)) |>
      dplyr::transmute(tmdb_id, company_id = id, company = name, origin_country),
    film_countries = explode(movies, function(m) m$production_countries,
                             list(iso_3166_1 = chr, name = chr)) |>
      dplyr::transmute(tmdb_id, country_code = iso_3166_1, country = name)
  )
}
