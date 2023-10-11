#' Read in survey data save as rda
#' 
#' atlantosom output is accessed and surveys pulled over time
#' 
#'@param atlmod configuration file specifying Atlantis simulation model filenames 
#'and locations  
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{year}{year simulated survey conducted}
#'\item{Code}{Atlantis model three letter code for predator group}
#'\item{Name}{Atlantis model common name for predator group}
#'\item{survey}{simulated survey name}
#'\item{pdlencm}{predator length bin (1 cm)}
#'\item{prey}{Atlantis model common name for prey group}
#'\item{variable}{proportion of prey in diet at predator length (dietprop)}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'

library(magrittr)

create_sim_survey_lendietcomp <- function(saveToData=T) {

  #uses existing mskeyrun datasets, ensure they are up to date
  survagelen <- mskeyrun::simSurveyAgeLencomp
  survdiet <- mskeyrun::simSurveyDietcomp
  
  svagelenbin <- survagelen %>%
    #dplyr::mutate(species = Name) %>%
    #dplyr::mutate(year = year-fitstartyr) %>% #year starts at 1
    #dplyr::select(Name, year, survey, agecl, lenbin, value) %>%
    dplyr::select(-c(variable, units)) %>%
    #dplyr::left_join(modbins) %>%
    #dplyr::filter(modbin.min <= lenbin & lenbin < modbin.max) %>% #lenbin defined as lower
    dplyr::group_by(ModSim, Code, Name, survey, year, agecl, lenbin) %>%
    dplyr::summarise(sumlen = sum(value)) %>%
    dplyr::group_by(Name, survey, year, lenbin) %>%
    dplyr::mutate(propage = sumlen/sum(sumlen)) #proportion of each agecl contributing to lengthbin
  
  simSurveyLenDietcomp <- survdiet %>%
    #dplyr::mutate(species = Name) %>%
    #dplyr::mutate(year = year-fitstartyr) %>% #year starts at 1
    dplyr::left_join(svagelenbin) %>%
    dplyr::mutate(dietpropage = value*propage) %>% #reweight diets for lengthbins
    dplyr::group_by(ModSim, Code, Name, survey, year, lenbin, prey) %>%
    dplyr::summarise(dietsize = sum(dietpropage)) %>%
    #dplyr::filter(prey %in% unique(modbins$species)) %>% #drops prey that aren't our modeled species
    #tidyr::spread(prey, dietsize) %>%
    dplyr::ungroup() %>%
    #dplyr::filter(!is.na(sizebin)) 
    dplyr::mutate(variable = "dietprop",
                  units = "proportion") %>%
    dplyr::rename(value = dietsize, pdlencm = lenbin) %>%
    dplyr::select(ModSim, year, Code, Name, survey, pdlencm, prey, variable, value, units)
  
  
  
  if (saveToData) {
  
    usethis::use_data(simSurveyLenDietcomp, overwrite = TRUE)
  }
  
  return(simSurveyLenDietcomp)
  
  
}
