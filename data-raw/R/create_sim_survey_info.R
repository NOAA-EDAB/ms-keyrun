#' Read in survey info (cv, timing, area) and save as rda
#' 
#' Use atlantisom config files to make a lookup table for species and survey names to cv, 
#' timing, and spatial coverage
#' 
#'@param atlmod configuration file specifying Atlantis simulation model filenames 
#'and locations  
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{Code}{Atlantis model three letter code for functional group}
#'\item{Name}{Atlantis model common name for functional group}
#'\item{survey}{simulated survey name}
#'\item{cv}{survey coefficient of variation (cv) of biomass}
#'\item{survMonth}{survey month of year converted from Atlantis output timestep}
#'\item{survArea}{survey coverage of Atlantis model polygons; "All" or "Subset"}
#'

library(magrittr)

create_sim_survey_info <- function(atlmod,fitstart=NULL,fitend=NULL,saveToData=T) {

  # input is path to model config file for atlantisom
  source(atlmod)
  
  # path for survey and fishery config files
  cfgpath <- stringr::str_extract(atlmod, ".*config")
  
  #works because atlantis directory named for model and simulation
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modsim <- modpath[length(modpath)]
  
  # get config files for survey cv
  svcon <- list.files(path=cfgpath, pattern = "*survey*", full.names = TRUE)
  
  # read true list with run and biol pars, etc
  omlist_ss <- readRDS(file.path(d.name, paste0(scenario.name, "omlist_ss.rds")))
  
  codename <- tibble(Name = omlist_ss$species_ss, 
                         Code = omlist_ss$code_ss)
  
  # model timesteps, etc from omdimensions script
  source(paste0(cfgpath,"/omdimensions.R"), local = TRUE)
  
  # survey cv lookup from config files
  allsvinfo <- tibble::tibble()
  for(c in 1:length(svcon)){
    source(svcon[c], local = TRUE)
    surv_inf_n <- surv_cv %>% 
      dplyr::mutate(survey = survey.name,
                    survMonth = ceiling((survey_sample_time/timestep*365)/(365/12)),
                    survArea = ifelse(length(survboxes) == length(allboxes), "All", "Subset"))
    allsvinfo <- dplyr::bind_rows(allsvinfo, surv_inf_n)
  }
  
  simSurveyInfo <- allsvinfo %>%
    dplyr::rename(Name = species) %>%
    dplyr::left_join(codename) %>%
    dplyr::select(Code, Name, survey, cv, survMonth, survArea) %>%
    dplyr::arrange(Name)
  
  if (saveToData) {
    #saveRDS(focalSpecies,saveToRDS)
    usethis::use_data(simSurveyInfo, overwrite = TRUE)
  }
  
  return(simSurveyInfo)
  
  
}
