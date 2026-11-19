# Saves the names you typed into data/city/neighborhood_names.csv and
# data/city/district_names.csv (columns your_name and your_subtitle) into
# names/, so they carry over to future runs. From the project folder:
#   source("save_names.R")
# Then commit names/ in the Git tab so they're backed up.

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)
cfg <- cinehood_config()
save_names(cfg)
