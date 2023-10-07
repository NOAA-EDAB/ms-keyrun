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
#'\item{agecl}{age class of Atlantis functional group}
#'\item{prey}{Atlantis model common name for prey group}
#'\item{variable}{proportion in diet (dietprop)}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'

library(magrittr)

create_sim_survey_dietcomp_fw <- function(atlmod,fitstart=NULL,fitend=NULL,saveToData=T) {

  # input is path to model config file for atlantisom
  source(atlmod)
  
  # path for survey and fishery config files
  cfgpath <- stringr::str_extract(atlmod, ".*config")
  
  #works because atlantis directory named for model and simulation
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modsim <- modpath[length(modpath)]
  
  #read in survey annual age comp data
  all_diets <- atlantisom::read_savedsurvs(d.name, 'survDiet')
  
  # get config files -- needed?
  svcon <- list.files(path=cfgpath, pattern = "*survey*", full.names = TRUE)
  
  # read true list with run and biol pars, etc
  omlist_ss <- readRDS(file.path(d.name, paste0(scenario.name, "omlist_fw.rds")))
  
  # model timesteps, etc from omdimensions script
  source(paste0(cfgpath,"/omdimensions.R"), local = TRUE)
  
  #Number of years
  nyears <- omlist_ss$runpar$nyears
  total_sample <- omlist_ss$runpar$tstop/omlist_ss$runpar$outputstep
  
  # user specified fit start and times if different from full run
  fitstartyr <- ifelse(!is.null(fitstart), fitstart-1, 0)
  fitendyr <- ifelse(!is.null(fitend), fitend, total_sample)
  
  atlantis_full <- c(1:total_sample)  
  mod_burnin <- fitstartyr*stepperyr+1
  fit_nyears <- fitendyr-fitstartyr
  fit_ntimes <- fit_nyears*stepperyr
  fittimes <- atlantis_full[mod_burnin:(mod_burnin+fit_ntimes-1)]
  #fit_timesteps <- seq(fittimes[stepperyr], max(fittimes), by=stepperyr) #last timestep
  #fit_years <- unique(floor(fittimes/stepperyr)) #from Christine's new sardine_config.R
  fittimes.days <- if(omlist_ss$runpar$outputstepunit=="days") fittimes*omlist_ss$runpar$outputstep
  
  
  # # survey cv lookup from config files
  # svcvlook <- tibble::tibble()
  # for(c in 1:length(svcon)){
  #   source(svcon[c], local = TRUE)
  #   surv_cv_n <- surv_cv %>% 
  #     dplyr::mutate(survey=survey.name)
  #   svcvlook <- dplyr::bind_rows(svcvlook, surv_cv_n)
  # }
  
  allsvdietprop <- tibble::tibble()
  
  #multiple surveys named in list object
  for(s in names(all_diets)){
    #arrange into wide format: year, Species1, Species2 ... and write csv
    svdietprop <- all_diets[[s]][[1]] %>%
      dplyr::filter(time.days %in% fittimes.days) %>%
      dplyr::mutate(year = ceiling(time.days/365)) %>%
      dplyr::select(species, agecl, year, prey, dietprop=dietSamp) %>%
      dplyr::left_join(dplyr::select(omlist_ss$funct.group_ss, Code, Name), by = c("species" = "Name")) %>%
      dplyr::mutate(ModSim = modsim) %>%
      dplyr::mutate(survey = s) %>%
      #dplyr::left_join(svcvlook) %>%
      dplyr::select(ModSim, year, Code, Name=species, survey, agecl, everything()) %>%
      tidyr::pivot_longer(cols = c("dietprop"), 
                          names_to = "variable",
                          values_to = "value") %>%
      dplyr::mutate(units = ifelse(variable=="dietprop", "proportion", "NA")) %>%
      dplyr::arrange(Name, survey, variable, year, agecl)
    
    allsvdietprop <- dplyr::bind_rows(allsvdietprop, svdietprop)
  }
  
  simSurveyDietcompFW <- allsvdietprop
  
  if (saveToData) {
  
    usethis::use_data(simSurveyDietcompFW, overwrite = TRUE)
  }
  
  return(simSurveyDietcompFW)
  
  
}
