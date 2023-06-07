#' Create fishery index by gear for ms-keyrun project
#' 
#' CURRENTLY A TEMPORARY FIX FOR NAs IN CATCH INDEX FOR SIMULATIONS
#' DO NOT PUT THIS IN MSKEYRUN MAIN BRANCH UNTIL NAs IN SOURCE DATA FIXED
#' DO NOT USE THIS AS ACTUAL GEORGES BANK DATA
#'
#'#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{variable}{commercial landings or commercial discards}
#'\item{modelName}{species modelName as specified in\code{mskeyrun::focalSpecies} }
#'\item{YEAR}{year of catch}
#'\item{hydraFleets}{3 fleets for the hydra model as specified in \code{mskeyrun::fleets}}
#'\item{valueNAfill}{estimated landings or discards with NAs filled according to proportions by non-na gear}
#'\item{units}{metric tons}
#'


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

######## START TEMP FIX ###############

# SKG June 2023 TEMPORARY FIX UNTIL ISSUE CAUSING NAs IS RESOLVED

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
catchIndexComlandrGearNAfill <- catchIndexGear %>%
  dplyr::filter(!is.na(Fleet)) %>%
  dplyr::bind_rows(fillNAgear) %>%
  dplyr::group_by(variable, modelName, YEAR, Fleet) %>%
  dplyr::summarise(valueNAfill = sum(value))
  
######## END TEMP FIX ###############

#Aggregate to 3 gears for hydra: demersal, fixedGear, pelagic 

# link up mskeyrun hydraFleets and comlandr fleet
fleetlook <- merge(mskeyrun::fleets, comlandr::mskeyGears) %>%
  dplyr::select(Fleet, hydraFleets) %>%
  dplyr::distinct()

# still a temporary data object until NAs fixed
catchIndexGearNAfill <- catchIndexComlandrGearNAfill %>%
  dplyr::left_join(fleetlook) %>%
  dplyr::group_by(variable, modelName, YEAR, hydraFleets) %>%
  dplyr::summarise(valueNAfill = sum(valueNAfill)) %>%
  dplyr::mutate(units = "metric tons")
  

#Output to package
usethis::use_data(catchIndexGearNAfill, overwrite = TRUE)
