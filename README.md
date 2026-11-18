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
| 1a | `R/ingest_ids.R` | Downloads TMDB's daily export of every movie ID; keeps the 40,000 most popular non-adult, non-video entries |
| 1b | `R/discover.R` | The second way in: searches TMDB year by year for every film with enough votes (50+ before 1970, 100+ before 1990, 200+ after), so older films with little current activity aren't missed. Merged with 1a; each candidate is labeled by how it got in |
| 2 | `R/fetch_movies.R` | Fetches each film's details, credits, keywords and release dates in one request; caches the JSON; retries rate limits and errors one at a time |
| 3 | `R/parse_movies.R` | Turns the JSON into the tables above |
| 4 | `R/derive.R` | Keeps released films of 40+ minutes with enough votes for their era (75+ before 1970, 150+ before 1990, 300+ after); adds decade, runtime bucket, log votes, and the public baseline |
| 5 | `R/checks.R` | Writes the report |

All the knobs (how many candidates, the vote-count search floor and first year, the floors, cast size, request rate) are in `R/config.R`.

### The public baseline

TMDB ratings are noisy for films with few votes, so `public_wr` is a weighted rating that pulls thin films toward the catalog average:

`public_wr = v/(v+m) × rating + m/(v+m) × catalog mean`, with `m` = 300 for every film, so an older film kept on fewer votes is pulled harder toward the average.

`public_z` is that, standardized across the kept catalog. It's the "tourists say" number the taste model compares you against.

### Tests

```r
testthat::test_dir("tests/testthat")
```

They run on made-up fixture films in `tests/testthat/fixtures/`, no API calls.

## Phase 2: the city

Groups the catalog into **neighborhoods** (micro-genres) and **districts** (groups of neighborhoods), and lays every film out on a 2D map.

```r
source("setup.R")        # once: adds Matrix, irlba, uwot, dbscan, ggplot2
source("run_phase2.R")   # or targets::tar_make(), which runs both phases
```

It takes a few minutes. It reads phase 1's tables, so run phase 1 first.

### How it works

| Step | What it does |
| --- | --- |
| Features | Each film described by its keywords (TF-IDF, the main signal), genres, release year, language and runtime. **No directors**: neighborhoods are about what films are like, not who made them. Production keywords ("sequel", "woman director", "based on novel or book") are left out |
| Compress | Keywords are compressed on their own (truncated SVD, 100 dimensions), then genre, language, year and runtime are added back at light weights so they nudge rather than dominate |
| Layout | UMAP twice: 10 dimensions to find neighborhoods in, 2 for the map |
| Neighborhoods | HDBSCAN finds dense groups (15+ films). Films it leaves unclaimed move to the nearest neighborhood and are flagged |
| Districts | Neighborhoods whose centers sit close together are grouped into 20 districts |
| Names | Draft names from each neighborhood's most distinctive keywords (common inside it, rarer outside) |

### What comes out

In `data/city/`:

| File | What it holds |
| --- | --- |
| `film_neighborhoods` | Each film's neighborhood, district, map position (x, y), and whether it was placed after being left unclaimed |
| `neighborhoods` | Each neighborhood: draft name, size, district, years, top genres, top keywords, example films, map center |
| `districts` | The same for districts |
| `cluster_space` | Each film's 10-dimensional position, used later to place new films |
| `neighborhood_names.csv` | A sheet for naming by hand: fill in `your_name` |

And in `data/checks/`: `city_report.md` (summary, districts, biggest neighborhoods, and 20 random neighborhoods to check by eye) and `city_map.png`.

All settings are in `R/config.R` under "Phase 2". `tools/check_city_scale.R` tests the method on a made-up catalog the size of the real one.

## Data and attribution

Film data comes from [TMDB](https://www.themoviedb.org/). This product uses the TMDB API but is not endorsed or certified by TMDB. TMDB's API is free for non-commercial use; commercial use needs a separate agreement with TMDB. Cached TMDB data must not be kept longer than six months, which the refresh window in `R/config.R` enforces.
