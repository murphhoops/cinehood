# Cinehood

Movie recommendations built on how your taste differs from the crowd's. This repo holds the data pipeline and, later, the app.

## Phase 1: the data spine

Builds the film catalog everything else reads from: one table of films, plus tables of their genres, people, keywords, companies and countries, all keyed on TMDB's movie ID.

### Setup (once)

1. Install the packages:
   ```r
   source("setup.R")
   ```
2. Get a TMDB **API Read Access Token**: make a free account at themoviedb.org, then Settings → API. It's the long token, not the short API key.
3. Put it in your user `.Renviron`, then restart R:
   ```r
   usethis::edit_r_environ()
   # add this line, with your token:
   # TMDB_TOKEN=eyJhbGciOi...
   ```
   Never commit the token. `.Renviron` in this folder is gitignored in case you put it here instead.

### Run it

```r
targets::tar_make()        # with {targets}
# or
source("run_phase1.R")     # plain script, same steps
```

The first run fetches details for about 40,000 films at ~35 requests a second, so it takes roughly 20 minutes. Every response is cached as raw JSON under `data/raw/movies/`, so an interrupted run picks up where it stopped and later runs only fetch what's new or older than six months.

### What comes out

In `data/spine/` (Parquet, or `.rds` if `{arrow}` isn't installed):

| Table | One row per | Key columns |
| --- | --- | --- |
| `films` | film | title, year, decade, runtime, runtime_bucket, original_language, vote_average, vote_count, public_wr, public_z, us_certification, collection, overview |
| `film_genres` | film × genre | genre |
| `film_people` | film × person | role (director, writer, cast), billing, detail (job or character) |
| `film_keywords` | film × keyword | keyword |
| `film_companies` | film × company | company, origin_country |
| `film_countries` | film × country | country |
| `keyword_counts` | keyword | how many kept films carry it |

And `data/checks/spine_report.md`: counts at each step, how complete each field is, and the spread of years, votes, languages, decades and genres.

### The steps

| Step | File | What it does |
| --- | --- | --- |
| 1 | `R/ingest_ids.R` | Downloads TMDB's daily export of every movie ID; keeps the 40,000 most popular non-adult, non-video entries |
| 2 | `R/fetch_movies.R` | Fetches each film's details, credits, keywords and release dates in one request; caches the JSON; retries rate limits and errors one at a time |
| 3 | `R/parse_movies.R` | Turns the JSON into the tables above |
| 4 | `R/derive.R` | Keeps released films of 40+ minutes and 300+ votes; adds decade, runtime bucket, log votes, and the public baseline |
| 5 | `R/checks.R` | Writes the report |

All the knobs (how many candidates, the floors, cast size, request rate) are in `R/config.R`.

### The public baseline

TMDB ratings are noisy for films with few votes, so `public_wr` is a weighted rating that pulls thin films toward the catalog average:

`public_wr = v/(v+m) × rating + m/(v+m) × catalog mean`, with `m` = the vote floor.

`public_z` is that, standardized across the kept catalog. It's the "tourists say" number the taste model compares you against.

### Tests

```r
testthat::test_dir("tests/testthat")
```

They run on made-up fixture films in `tests/testthat/fixtures/`, no API calls.

## Data and attribution

Film data comes from [TMDB](https://www.themoviedb.org/). This product uses the TMDB API but is not endorsed or certified by TMDB. TMDB's API is free for non-commercial use; commercial use needs a separate agreement with TMDB. Cached TMDB data must not be kept longer than six months, which the refresh window in `R/config.R` enforces.
