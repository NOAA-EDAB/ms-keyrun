#' Pull focal Species for ms-keyrun project
#' 
#' Gets SVSPP, NESPP3, Species_itis, nafospp, scientific name, common name in one table
#'
#'@param channel Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{NESPP3}{code for secies found in commercial fishing database}
#'\item{SVSPP}{code for species found in bottom trawl survey database}
#'\item{NAFOSPP}{code for species in nafo database}
#'\item{SPECIES_ITIS}{itis code}
#'\item{COMMON_NAME}{common name from commercial fishing database}
#'\item{COMNAME}{common name from bottom trawl survey fishing database}
#'\item{SCIENTIFIC_NAME}{scientific name of species}
#'
#'


library(magrittr)

get_focal_species <- function(channel,saveToData=T) {
  
  # create lookup table for species of interest
  lookup <- dbutils::create_species_lookup(channel,species=c(32,73,197,74,15,106,105,121,72,23,75),speciesType = "SVSPP")
  focalSpecies <- lookup$data %>%
    dplyr::select(-SVSPPcf) %>%
    dplyr::rename(SVSPP=SVSPPsv) %>%
    dplyr::distinct(.)
  
  if (saveToData) {
    #saveRDS(focalSpecies,saveToRDS)
    usethis::use_data(focalSpecies)
  }
  
  return(focalSpecies)

}