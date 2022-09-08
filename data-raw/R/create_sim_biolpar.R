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
library(sicegar)

create_sim_biolpar <- function(atlmod,saveToData=T) {
  # create lookup table for simulated species of interest
  source(atlmod)
  
  # read true list with run and biol pars, etc
  omlist_ss <- readRDS(file.path(d.name, paste0(scenario.name, "omlist_ss.rds")))
  
  modsim <- stringr::str_split(d.name, "/", simplify = TRUE)
  modsim <- modsim[length(modsim)]
  
  simBiolPar <- omlist_ss$funct.group_ss %>% 
    dplyr::left_join(omlist_ss$biol$wl, by=c("Code"="group")) %>%
    dplyr::arrange(Name) %>%
    dplyr::mutate(ModSim = modsim) %>%
    dplyr::select(ModSim, Code, Name, WLa = a, WLb = b) %>%
    dplyr::arrange(Name)
  
  # do von B here also? no, requires fitting to different eras
  # maybe maturity pars though
  # Aug 2022 this is a start but not currently needed so skip
  
  maturitypar <- omlist_ss$funct.group_ss %>% 
    dplyr::left_join(omlist_ss$biol$maturityogive, by=c("Code"="code")) %>%
    dplyr::arrange(Name) %>%
    dplyr::mutate(ModSim = modsim) %>%
    dplyr::select(ModSim, Code, Name, NumCohorts, NumAgeClassSize, agecl1:agecl9, agecl10 = " agecl10") %>%
    tidyr::pivot_longer(cols = agecl1:agecl10, names_to ="agecl", values_to = "propmat") %>%
    dplyr::mutate(agecl = readr::parse_number(as.character(agecl)))
  # fit logistic model to maturity at agecl ogive
  # convert to parameters for maturity at length: requires VB fits!
  # return parameters as inputs
  
  
  if (saveToData) {
    #saveRDS(focalSpecies,saveToRDS)
    usethis::use_data(simBiolPar, overwrite = TRUE)
  }
  
  return(simBiolPar)
}