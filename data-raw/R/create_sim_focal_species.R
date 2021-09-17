#' Pull simulatead data focal Species for ms-keyrun project
#' 
#' Gets atlantis Code, scientific name, and Name (common name) in one table
#'
#'@param atlmod configuration file specifying Atlantis simulation model filenames 
#'and locations  
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{Code}{Atlantis model three letter code for functional group}
#'\item{Name}{Atlantis model common name for functional group}
#'\item{SciName}{scientific name of species}
#'

library(magrittr)

create_sim_focal_species <- function(atlmod,saveToData=T) {
  
  # create lookup table for simulated species of interest
  source(atlmod)
  
  fgs <- atlantisom::load_fgs(d.name, functional.groups.file)
  
  modsim <- stringr::str_split(d.name, "/", simplify = TRUE)
  modsim <- modsim[length(modsim)]
  
  #hardcoded because not part of fgs
  lname <- data.frame(SciName = c("*Hippoglossoides platessoides*",
                                "*Reinhardtius hippoglossoides*",
                                "*Scomber scombrus*",
                                "*Melongrammus aeglefinus*",
                                "*Pollachius virens*",
                                "*Sebastes mentella*",
                                "*Micromesistius poutassou*",
                                "*Clupea harengus*",
                                "*Gadus morhua*",
                                "*Boreogadus saida*",
                                "*Mallotus villosus*"),
                      Code = c("LRD", "GRH", "MAC", "HAD", "SAI", "RED", 
                               "BWH", "SSH", "NCO", "PCO", "CAP")
  )
  
  sppsubset <- merge(fgs, lname, all.y = TRUE)
  simFocalSpecies <- sppsubset %>% 
    dplyr::arrange(Index) %>%
    dplyr::mutate(ModSim = modsim) %>%
    dplyr::select(ModSim, Code, Name, SciName)
  
  if (saveToData) {
    #saveRDS(focalSpecies,saveToRDS)
    usethis::use_data(simFocalSpecies)
  }
  
  return(simFocalSpecies)
  
}