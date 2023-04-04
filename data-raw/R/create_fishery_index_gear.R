#Create landings index
# Modified from Sean Lucey's code by Sarah G April 2023
# landings.RData and discards.RData are local files, not for github
#For a complete update run pull_comlands.R to generate new comland data set
library(here); library(survdat); library(data.table); library(sf)

#Load landings data
load(here::here('data-raw', 'data', 'landings.RData'))

#Load discards data
load(here::here('data-raw', 'data', 'discards.RData'))

#Sum over all catch variables for total catch
spp <- as.numeric(unique(mskeyrun::focalSpecies$NESPP3))

#Landings
landIndex <- landings$comland[NESPP3 %in% spp, .(value = sum(SPPLIVMT)),
                              by = c('NESPP3', 'Fleet', 'YEAR')]
landIndex[, variable := 'commercial landings']
landIndex[, units    := 'metric tons']

#Discards
discIndex <- discards$comdisc[NESPP3 %in% spp, .(value = sum(DISMT, na.rm = T)),
                              by = c('NESPP3', 'Fleet', 'YEAR')]
discIndex[, variable := 'commercial discards']
discIndex[, units := 'metric tons']

#Combine landings and discards by gear
catchIndexGear <- data.table::rbindlist(list(landIndex, discIndex))

#Output to package
usethis::use_data(catchIndexGear, overwrite = TRUE)
