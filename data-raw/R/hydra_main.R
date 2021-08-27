#' create hydra data for package in rda format 
#'
#' As outlined in GBLandingsByLength.Rmd
#'
#'
#'
#'
#'
#'
#'

channel <- dbutils::connect_to_database("server","user")

source(here::here("data-raw/R","hydra_pull_GB_lengths.R"))
source(here::here("data-raw/R","hydra_process_GB_comland.R"))
source(here::here("data-raw/R","hydra_format.R"))

hydra_main <- function(channel){
  
  #pull comland data
  comlandData <- readRDS(here::here("data-raw/data","comland_negear.rds"))
  comlandData <- comlandData$comland
  # get lengths in GB from cfdbs 
  lengthData <- hydra_pull_GB_lengths(channel, area=c(cfdbs::EPUs$data$GB,537))
  
  # expand lengths as in mscatch
  expanded <- hydra_process_GB_comland(channel,comlandData,lengthData)
  
  # for each species format the result
  speciesList <- unique(expanded$species_itis)
  catchAtLengthProportions <- NULL
  for (itis in speciesList) {
    itisData <- hydra_format(expanded,mscatch::fleets,itis)
    catchAtLengthProportions <- rbind(catchAtLengthProportions,itisData)
  }
  
  # save final table
  usethis::use_data(catchAtLengthProportions)
  
}