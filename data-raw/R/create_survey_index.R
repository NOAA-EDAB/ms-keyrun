#Create survey index
#For a complete update run pull_survdat.R to generate new survdat data sets
library(here); library(survdat); library(data.table); library(sf)

#Load seperete albatross and bigelow data sets
load(here::here('data-raw', 'data', 'survdat_Albatross.RData'))
load(here::here('data-raw', 'data', 'survdat_Bigelow.RData'))

#Focal species
spp <- c(32, 73, 197, 74, 15, 106, 105, 121, 72, 23, 75)

#Load Georges Bank survey strata
GB <- read_sf(here('data-raw', 'gis'), 'GB_SOE_strata')

#Focal species
surveyIndexA4 <- survdat::calc_stratified_mean(survdat.a4$survdat, 
                                               areaPolygon = GB,
                                               areaDescription = 'EPU',
                                               filterBySeason = c('FALL', 'SPRING'),
                                               filterByGroup = spp, tidy = T)

surveyIndexHB <- survdat::calc_stratified_mean(survdat.hb$survdat, 
                                               areaPolygon = GB,
                                               areaDescription = 'EPU',
                                               filterBySeason = c('FALL', 'SPRING'),
                                               filterByGroup = spp, tidy = T)

#All species
surveyIndexA4All <- survdat::calc_stratified_mean(survdat.a4$survdat, 
                                               areaPolygon = GB,
                                               areaDescription = 'EPU',
                                               filterBySeason = c('FALL', 'SPRING'),
                                               tidy = T)

surveyIndexHBAll <- survdat::calc_stratified_mean(survdat.hb$survdat, 
                                               areaPolygon = GB,
                                               areaDescription = 'EPU',
                                               filterBySeason = c('FALL', 'SPRING'),
                                               tidy = T)

#Output to package
usethis::use_data(surveyIndexA4, overwrite = TRUE)
usethis::use_data(surveyIndexHB, overwrite = TRUE)
usethis::use_data(surveyIndexA4All, overwrite = TRUE)
usethis::use_data(surveyIndexHBAll, overwrite = TRUE)

