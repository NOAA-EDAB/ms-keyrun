#Generate new EPU shapefile based on SOE strata sets
library(sf); library(here); library(ggplot2); library(dplyr)

strata <- read_sf(here('gis'), 'strata')

GB <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)

GB.soe <- dplyr::filter(strata, STRATA %in% GB)

GB.soe$EPU <- 'GB'

GB.soe <-
    GB.soe %>%
    summarise(EPU = 'GB')

ggplot(GB.soe) + geom_sf()

st_write(GB.soe, here('gis/GB_SOE_strata.shp'))

test2 <- read_sf(here('gis'), 'GB_SOE_strata')
