#' Read in fishery data save as rda
#' 
#' atlantisom output is accessed and fishery weight at age pulled over time
#' 
#'@param atlmod configuration file specifying Atlantis simulation model filenames 
#'and locations  
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{year}{year simulated fishery conducted}
#'\item{Code}{Atlantis model three letter code for functional group}
#'\item{Name}{Atlantis model common name for functional group}
#'\item{fishery}{simulated fishery name}
#'\item{age}{annual age of Atlantis functional group}
#'\item{variable}{mean weight at age (Wtage)}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'

library(magrittr)

create_sim_fishery_wtage <- function(atlmod,fitstart=NULL,fitend=NULL,saveToData=T) {

  # input is path to model config file for atlantisom
  source(atlmod)
  
  # path for survey and fishery config files
  cfgpath <- stringr::str_extract(atlmod, ".*config")
  
  #works because atlantis directory named for model and simulation
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modsim <- modpath[length(modpath)]
  
  #read in fishery annual mean wt at age data
  fish_wtage <- atlantisom::read_savedfisheries(d.name, 'catchWtage')
  fish_annage_wtage <- atlantisom::read_savedfisheries(d.name, 'catchAnnWtage')
  
  #join to get a full set of annual wt at age
  # by fishery, make same list structure, add this to atlantisom?
  for(f in names(fish_wtage)){
    
    # these species didn't need interpolation because they are annual
    nointerp <- fish_wtage[[f]][[1]] %>%
      dplyr::anti_join(fish_annage_wtage[[f]][[1]], by="species")
    
    # bind them back to annage dataset because they are annual
    fish_annage_wtage[[f]][[1]] <- fish_annage_wtage[[f]][[1]] %>%
      dplyr::bind_rows(nointerp)
    
  }
  
  # get config files -- needed?
  fvcon <- list.files(path=cfgpath, pattern = "*fishery*", full.names = TRUE)
  
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
  #fit_years <- unique(floor(fittimes/stepperyr)) #from Christine's new sardine_config.R
  fittimes.days <- if(omlist_ss$runpar$outputstepunit=="days") fittimes*omlist_ss$runpar$outputstep
  
  # # fishery cv lookup from config files
  # fcvlook <- tibble::tibble()
  # for(c in 1:length(fishcon)){
  #   source(fishcon[c], local = TRUE)
  #   fish_cv_n <- fish_cv %>%
  #     dplyr::mutate(fishery=fishery.name)
  #   fcvlook <- dplyr::bind_rows(fcvlook, fish_cv_n)
  # }
  
  allfishwtage <- tibble::tibble()
  
  #multiple surveys named in list object
  for(f in names(fish_annage_wtage)){
    #arrange into wide format: year, Species1, Species2 ... and write csv
    fishwtage <- fish_annage_wtage[[f]][[1]] %>%
      dplyr::filter(time %in% fittimes) %>%
      dplyr::mutate(year = ceiling(time/stepperyr)) %>%
      dplyr::select(species, year, agecl, atoutput) %>%
      dplyr::group_by(species, year, agecl) %>%
      dplyr::summarise(Wtage = mean(atoutput, na.rm=T)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(dplyr::select(omlist_ss$funct.group_ss, Code, Name), by = c("species" = "Name")) %>%
      dplyr::mutate(ModSim = modsim) %>%
      dplyr::mutate(fishery = f) %>%
      #dplyr::left_join(svcvlook) %>%
      dplyr::select(ModSim, year, Code, Name=species, fishery, age=agecl, everything()) %>%
      tidyr::pivot_longer(cols = c("Wtage"), 
                          names_to = "variable",
                          values_to = "value") %>%
      dplyr::mutate(units = ifelse(variable=="Wtage", "g", "NA")) %>%
      dplyr::arrange(Name, fishery, variable, year, age)
    
    allfishwtage <- dplyr::bind_rows(allfishwtage, fishwtage)
  }
  
  simFisheryWtatAge <- allfishwtage
  
  if (saveToData) {
  
    usethis::use_data(simFisheryWtatAge, overwrite = TRUE)
  }
  
  return(simFisheryWtatAge)
  
  
}
