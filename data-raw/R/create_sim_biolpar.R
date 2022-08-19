#' Pull simulatead biological parameters for focal Species for ms-keyrun project
#' 
#' Gets Atlantis Code, Name (common name), length-weight and other pars in one table
#'
#'@param atlmod configuration file specifying Atlantis simulation model filenames 
#'and locations  
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{Code}{Atlantis model three letter code for functional group}
#'\item{Name}{Atlantis model common name for functional group}
#'\item{WLa}{Weight-Length equation parameter a, W = aL^b}
#'\item{WLb}{Weight-Length equation parameter b, W = aL^b}
#'

library(magrittr)

create_sim_biolpar <- function(atlmod,saveToData=T) {
  # create lookup table for simulated species of interest
  source(atlmod)
  
  # read true list with run and biol pars, etc
  omlist_ss <- readRDS(file.path(d.name, paste0(scenario.name, "omlist_ss.rds")))
  
  modsim <- stringr::str_split(d.name, "/", simplify = TRUE)
  modsim <- modsim[length(modsim)]
  
  simBiolPar <- omlist_ss$biol$wl %>% 
    dplyr::arrange(Index) %>%
    dplyr::mutate(ModSim = modsim) %>%
    dplyr::select(ModSim, Code, Name, a, b) %>%
    dplyr::arrange(Name)
  
  # do von B here also?
  
  
  if (saveToData) {
    #saveRDS(focalSpecies,saveToRDS)
    usethis::use_data(simBiolPar, overwrite = TRUE)
  }
  
  return(simBiolPar)
}