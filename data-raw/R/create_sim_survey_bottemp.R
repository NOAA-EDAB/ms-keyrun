#' Read in survey temperature data save as rda
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
#'\item{survey}{simulated survey name}
#'\item{variable}{mean bottom temperature degrees C}
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
  
  ## current temp stuff
  # taken from existing load_nc to explore
  # file.nc <- file.path(d.name, "outputnordic_runresults_01.nc")
  # 
  #   # Load ATLANTIS output!
  #   at_out <- RNetCDF::open.nc(con = file.nc)
  # 
  #   # Get info from netcdf file! (Filestructure and all variable names)
  #   var_names_ncdf <- sapply(seq_len(RNetCDF::file.inq.nc(at_out)$nvars - 1),
  #     function(x) RNetCDF::var.inq.nc(at_out, x)$name)
  #   n_timesteps <- RNetCDF::dim.inq.nc(at_out, 0)$length
  #   n_boxes     <- RNetCDF::dim.inq.nc(at_out, 1)$length
  #   n_layers    <- RNetCDF::dim.inq.nc(at_out, 2)$length
  # 
  # RNetCDF::close.nc(at_out)
  
  #'@physic_variables A character value spefifying which variables
  #'   should be loaded. Only one variable can be loaded at a time.
  #'   Currently, only the following variables are allowed:
  #'   "salt", "NO3", "NH3", "Temp", "Oxygen", "Si", "Det_Si", "DON",
  #'    "Chl_a", "Denitrifiction", "Nitrification".
  
  truetemp <- atlantisom::load_nc_physics(dir = d.name,
                                          file_nc = "nordic_runresults_01.nc",
                                          physic_variables = "Temp",
                                          aggregate_layers = FALSE,
                                          bboxes = atlantisom::get_boundary(NOBAom_ms$boxpars))
  #if(verbose) message("Temperature read in.")
  
  # this averages all depth layers, don't want this
  # truetempagg <- atlantisom::load_nc_physics(dir = d.name,
  #                   file_nc = "outputnordic_runresults_01.nc",
  #                   physic_variables = "Temp",
  #                   aggregate_layers = TRUE,
  #                   bboxes = atlantisom::get_boundary(NOBAom_ms$boxpars))
  
  truebottomtemp <- truetemp %>%
    filter(layer == 0) # according to Atlantis wiki, layer 0 is bottom
  
  truesurfacetemp <- truetemp %>%
    filter(layer<7) %>%  #layer 7 is always identical temp to layer 0, may be the added sediment layer?
    group_by(polygon, time) %>%
    filter(layer==max(layer)) %>%
    ungroup
  
  surveyenv <- function(trueenv, 
                        survboxes,
                        survtimes) {
    surveydat <- trueenv %>%
      dplyr::filter(polygon %in% survboxes) %>%
      dplyr::filter(time %in% survtimes)
    
    return(surveydat)
  }
  
  omlist_ss <- NOBAom_ms
  source(here("SkillAssessment/config/omdimensions.R"))
  
  #need to weight by polygon area to get mean annual temp time series for each survey
  boxarea <- map_dfr(NOBAom_ms$boxpars$boxes, "area") %>%
    pivot_longer(everything(), names_to = "polygon", values_to = "area") %>%
    mutate(polygon = as.integer(polygon)) %>%
    mutate(proparea = area/sum(area))
  
  
  source(here("SkillAssessment/config/mssurvey_fall.R"))
  
  fallcensus_bottomtemp <- surveyenv(truebottomtemp, survboxes = survboxes, survtimes = survtime)
  
  fallcensus_btempindex <- left_join(fallcensus_bottomtemp, boxarea) %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(meantemp = weighted.mean(atoutput, proparea)) %>%
    dplyr::filter(time %in% fittimes) %>%
    dplyr::mutate(year = floor(time/stepperyr)) %>%
    dplyr::select(year, meantemp) %>%
    write_csv(paste0(o.name,"observation_temperature_NOBA_",survey.name,".csv"))
  
  source(here("SkillAssessment/config/mssurvey_spring.R"))
  
  springcensus_bottomtemp <- surveyenv(truebottomtemp, survboxes = survboxes, survtimes = survtime)
  
  springcensus_btempindex <- left_join(springcensus_bottomtemp, boxarea) %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(meantemp = weighted.mean(atoutput, proparea)) %>%
    dplyr::filter(time %in% fittimes) %>%
    dplyr::mutate(year = floor(time/stepperyr)) %>%
    dplyr::select(year, meantemp) %>%
    write_csv(paste0(o.name,"observation_temperature_NOBA_",survey.name,".csv"))
  ### end script replace bio below with the above
  
  #read in survey biomass data
  survObsBiom <- atlantisom::read_savedsurvs(d.name, 'survB') #reads in all surveys
  
  # get config files for survey cv
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
  
  
  # survey cv lookup from config files
  svcvlook <- tibble::tibble()
  for(c in 1:length(svcon)){
    source(svcon[c], local = TRUE)
    surv_cv_n <- surv_cv %>% 
      dplyr::mutate(survey=survey.name)
    svcvlook <- dplyr::bind_rows(svcvlook, surv_cv_n)
  }
  
  allsvbio <- tibble::tibble()
  
  #multiple surveys named in list object
  for(s in names(survObsBiom)){
    #arrange into wide format: year, Species1, Species2 ... and write csv
    svbio <- survObsBiom[[s]][[1]] %>%
      dplyr::filter(time %in% fittimes) %>%
      dplyr::mutate(year = floor(time/stepperyr)) %>%
      dplyr::select(species, year, atoutput) %>%
      dplyr::rename(biomass = atoutput) %>%
      dplyr::left_join(dplyr::select(omlist_ss$funct.group_ss, Code, Name), by = c("species" = "Name")) %>%
      dplyr::mutate(ModSim = modsim) %>%
      dplyr::mutate(survey = s) %>%
      dplyr::left_join(svcvlook) %>%
      dplyr::select(ModSim, year, Code, Name=species, survey, everything()) %>%
      tidyr::pivot_longer(cols = c("biomass", "cv"), 
                          names_to = "variable",
                          values_to = "value") %>%
      dplyr::mutate(units = ifelse(variable=="biomass", "tons", "unitless")) %>%
      dplyr::arrange(Name, survey, variable, year)
    
    allsvbio <- dplyr::bind_rows(allsvbio, svbio)
  }
  
  simSurveyIndex <- allsvbio
  
  if (saveToData) {
    #saveRDS(focalSpecies,saveToRDS)
    usethis::use_data(simSurveyIndex, overwrite = TRUE)
  }
  
  return(simSurveyIndex)
  
  
}
