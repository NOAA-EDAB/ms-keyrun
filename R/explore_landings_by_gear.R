#' Explore landings 
#' 
#' Explores time series of species caught by gear type
#' 
#' @param data Data frame. landings data by YEAR, NESPP3, NEGEAR2 (long format)
#' @param gearCode Numeric. NEGEAR2 code
#' @param filterYrs Numeric. A proportion reflecting the proportion of years containing data for a particucar species (eg.)
#' A value of .15 retains only the species in which there were landings in more that 15% of the years sampled
#' 
#' @return 
#' 
#' @example 
#' \dontrun{
#' }
#' 
#' 

library(magrittr)
library(ggplot2)
gearData <- readRDS(file=paste0(here::here("data","timeSeriesSpeciesByGear.rds")))

explore_landings_by_gear <- function(data=gearData$data,gearCode=5,filterYrs=0.30){
  
  # pull gear Specific data
  gd <- data %>%
    dplyr::filter(NEGEAR2==gearCode) %>%
    dplyr::mutate(NESPP3 = as.numeric(NESPP3),YEAR=as.numeric(YEAR))
  
  # yr range
  yrRange <- range(as.numeric(gd$YEAR))

  # find species that are not abundant
  filteredSpecies <- gd %>% 
    dplyr::group_by(NESPP3) %>%
    dplyr::summarise(n2=dplyr::n()) %>% 
    dplyr::filter(n2 > filterYrs*diff(yrRange))
  
  speciesLeftOut <- sort(setdiff(unique(gd$NESPP3),filteredSpecies$NESPP3))
  print(speciesLeftOut)
  
  # filter out species from data set
  gd2 <- gd %>% 
    dplyr::filter(NESPP3 %in% filteredSpecies$NESPP3)
  
  # calculate % of landings lost due to filtering out non abundant species
  landingsLeftOut <- gd %>% dplyr::filter(NESPP3 %in% speciesLeftOut) %>%
    dplyr::select(totsplandlb) %>% 
    sum()
  totalLandings <- gd %>% 
    dplyr::select(totsplandlb) %>% 
    sum()
  
  message(paste0("Percentage of landings due to omitted species = ",100*landingsLeftOut/totalLandings))
  
  p <- ggplot(data=gd2) +
    geom_line(mapping=aes(x=YEAR,y=totsplandlb)) +
    facet_wrap(vars(NESPP3),scales="fixed") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  plot(p)
  
  
}
