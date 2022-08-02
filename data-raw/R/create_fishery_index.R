#Create landings index
#For a complete update run pull_comlands.R to generate new comland data set
library(here); library(survdat); library(data.table); library(sf)

#Load landings data
load(here::here('data-raw', 'data', 'comland.RData'))

#Sum over all catch variables for total catch
spp <- as.numeric(unique(mskeyrun::focalSpecies$NESPP3))
catchIndex <- comland$comland[NESPP3 %in% spp, .(value = sum(SPPLIVMT)),
                              by = c('NESPP3', 'YEAR')]
catchIndex[, variable := 'commerical landings']
catchIndex[, units    := 'metric tons']

#Output to package
usethis::use_data(catchIndex, overwrite = TRUE)
