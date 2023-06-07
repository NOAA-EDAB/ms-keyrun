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

# TEMPORARY FIX UNTIL ISSUE CAUSING NAs IS RESOLVED
#Rescale assuming NA proportional to identified proportion
# extract proportion by gear with NA removed by land/disc, spp, year
propgear <- catchIndexGear %>%
  dplyr::filter(!is.na(Fleet)) %>%
  dplyr::group_by(variable, modelName, YEAR, Fleet) %>%
  dplyr::summarise(hasgear = sum(value)) %>%
  dplyr::mutate(prop = hasgear/sum(hasgear)) %>%
  dplyr::ungroup()

# apply proportion to NA component
fillNAgear <- catchIndexGear %>%
  dplyr::filter(is.na(Fleet)) %>%
  dplyr::select(-Fleet) %>%
  dplyr::left_join(propgear) %>%
  dplyr::mutate(value = value*prop) %>%
  dplyr::select(-c(hasgear, prop)) 
  
# add NA fill back in with gear catch renaming value
catchIndexGearNAfill <- catchIndexGear %>%
  dplyr::filter(!is.na(Fleet)) %>%
  dplyr::bind_rows(fillNAgear) %>%
  dplyr::group_by(variable, modelName, YEAR, Fleet) %>%
  dplyr::summarise(valueNAfill = sum(value))
  

#Aggregate to 3 gears for hydra: demersal, fixedGear, pelagic 



#Output to package
usethis::use_data(catchIndexGearNAfill, overwrite = TRUE)
