#' Read in survey temperature data save as rda
#' 
#' atlantis and atlantisom output is accessed and surveys pulled over time
#' 
#'@param atlmod configuration file specifying Atlantis simulation model filenames 
#'and locations  
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{year}{year simulated survey conducted}
#'\item{survey}{simulated survey name}
#'\item{variable}{mean annual bottom temperature degrees C}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'

library(magrittr)

create_sim_survey_bottemp <- function(atlmod,fitstart=NULL,fitend=NULL,saveToData=T) {

  # input is path to model config file for atlantisom
  source(atlmod)
  
  # path for survey and fishery config files
  cfgpath <- stringr::str_extract(atlmod, ".*config")
  
  #works because atlantis directory named for model and simulation
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modsim <- modpath[length(modpath)]
  
  # get config files for surveys
  svcon <- list.files(path=cfgpath, pattern = "*survey*", full.names = TRUE)
  
  # read true list with run and biol pars, etc
  omlist_ss <- readRDS(file.path(d.name, paste0(scenario.name, "omlist_ss.rds")))
  
  # model timesteps, etc from omdimensions script
  source(paste0(cfgpath,"/omdimensions.R"), local = TRUE)
  
  # user specified fit start and times if different from full run
  fitstartyr <- ifelse(!is.null(fitstart), fitstart, 0)
  fitendyr <- ifelse(!is.null(fitend), fitend, total_sample)
  
  #Number of years
  nyears <- omlist_ss$runpar$nyears
  total_sample <- noutsteps-1
  atlantis_full <- c(0:total_sample)  
  mod_burnin <- fitstartyr*stepperyr+1
  fit_nyears <- fitendyr-fitstartyr
  fit_ntimes <- fit_nyears*stepperyr
  fittimes <- atlantis_full[mod_burnin:(mod_burnin+fit_ntimes-1)]
  #fit_timesteps <- seq(fittimes[stepperyr], max(fittimes), by=stepperyr) #last timestep
  fit_years <- unique(floor(fittimes/stepperyr)) #from Christine's new sardine_config.R
  #fittimes.days <- if(omlist_ss$runpar$outputstepunit=="days") fittimes*omlist_ss$runpar$outputstep
  
  
  # extract true atlantis temperature
  truetemp <- atlantisom::load_nc_physics(dir = d.name,
                                          file_nc = "nordic_runresults_01.nc",
                                          physic_variables = "Temp",
                                          aggregate_layers = FALSE,
                                          bboxes = atlantisom::get_boundary(omlist_ss$boxpars))

  #need to weight by polygon area to get mean annual temp time series for each survey
  boxarea <- purrr::map_dfr(omlist_ss$boxpars$boxes, "area") %>%
    tidyr::pivot_longer(everything(), names_to = "polygon", values_to = "area") %>%
    dplyr::mutate(polygon = as.integer(polygon)) %>%
    dplyr::mutate(proparea = area/sum(area))
  
  allsvtemp <- tibble::tibble()
  
  for(c in 1:length(svcon)){
    source(svcon[c], local = TRUE) #THIS CREATES DATA FOR ALL SURVEYS
    
    bottomtemp <- atlantisom::create_survey_env(truetemp, 
                                                survdepth = "bottom", 
                                                survboxes = survboxes, 
                                                survtimes = survtime)
    
    btempindex <- dplyr::left_join(bottomtemp, boxarea) %>%
      dplyr::group_by(time) %>%
      dplyr::summarise(meantemp = weighted.mean(atoutput, proparea)) %>%
      dplyr::filter(time %in% fittimes) %>%
      dplyr::mutate(year = floor(time/stepperyr)) %>%
      dplyr::select(year, meantemp) %>%
      dplyr::mutate(survey=survey.name) %>%
      dplyr::mutate(ModSim = modsim) %>%
      dplyr::mutate(variable = "bottomtemp") %>%
      dplyr::mutate(units = "degC") %>%
      dplyr::select(ModSim, year, survey, variable, value=meantemp, units)
    
    allsvtemp <- dplyr::bind_rows(allsvtemp, btempindex)
    
  }
 
  simSurveyBottemp <- allsvtemp
  
  if (saveToData) {
    #saveRDS(focalSpecies,saveToRDS)
    usethis::use_data(simSurveyBottemp, overwrite = TRUE)
  }
  
  return(simSurveyBottemp)
  
  
}
