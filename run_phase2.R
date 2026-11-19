# Phase 2: the city. Builds neighborhoods and districts from the phase 1
# spine. Run phase 1 first, then, from the project folder:
#   source("run_phase2.R")

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

cfg <- cinehood_config()

message("1/4 reading the spine")
spine <- lapply(c(films = "films", film_keywords = "film_keywords", film_genres = "film_genres"),
                read_table, dir = cfg$out_dir)

message("2/4 features, layout and neighborhoods (a few minutes)")
city <- build_city(spine, cfg)

message("3/4 describing neighborhoods and districts")
tabs <- city_tables(city, spine, cfg)
tabs <- apply_names(city, tabs, cfg)

message("4/4 writing tables, map and report")
write_city(city, tabs, cfg)
map <- city_map(city, tabs, cfg)
report <- city_report(city, tabs, cfg)
message("done: ", nrow(tabs$neighborhoods), " neighborhoods in ", nrow(tabs$districts),
        " districts. report at ", report, if (!is.na(map)) paste0(", map at ", map),
        ". names to edit: ", cfg$city_dir, "/neighborhood_names.csv and district_names.csv")
