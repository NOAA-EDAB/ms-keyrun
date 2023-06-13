#' Read in fishery catch data save as rda
#' 
#' atlantosom output is accessed and fishery data pulled over time
#' simulated fishery catches are cumulative, not snapshots
#' fishery total catch for the year is the sum of the months
#' fishery catch for the month is all catch reported since the last reporting month
#' 
#' also makes annual aggregate dataset for backwards compatibility
#' 
#'@param atlmod configuration file specifying Atlantis simulation model filenames 
#'and locations  
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{year}{year simulated fishery conducted}
#'\item{fishMonth}{month simulated fishery conducted}
#'\item{Code}{Atlantis model three letter code for functional group}
#'\item{Name}{Atlantis model common name for functional group}
#'\item{fishery}{simulated fishery name}
#'\item{variable}{catch or coefficient of variation (cv) of biomass}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'

library(magrittr)

create_sim_fishery_index <- function(atlmod,fitstart=NULL,fitend=NULL,saveToData=T) {

  # input is path to model config file for atlantisom
  source(atlmod)
  
  # path for survey and fishery config files
  cfgpath <- stringr::str_extract(atlmod, ".*config")
  
  #works because atlantis directory named for model and simulation
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modsim <- modpath[length(modpath)]
  
  #read in survey biomass data
  catchbio_ss <- atlantisom::read_savedfisheries(d.name, 'Catch')
  
  # get config files for fishery cv
  fishcon <- list.files(path=cfgpath, pattern = "*fishery*", full.names = TRUE)
  
  # read true list with run and biol pars, etc
  omlist_ss <- readRDS(file.path(d.name, paste0(scenario.name, "omlist_ss.rds")))
  
  # model timesteps, etc from omdimensions script
  source(paste0(cfgpath,"/omdimensions.R"), local = TRUE)
  
  #Number of years
  nyears <- omlist_ss$runpar$nyears
  total_sample <- omlist_ss$runpar$tstop/omlist_ss$runpar$outputstep
  
  # throw an error if fstepperyr is not equal to stepperyr
  if(stepperyr != fstepperyr) stop("Error: check Atlantis timestep output for fishery")
  
  # user specified fit start and times if different from full run
  fitstartyr <- ifelse(!is.null(fitstart), fitstart-1, 0)
  fitendyr <- ifelse(!is.null(fitend), fitend, total_sample)
  
  atlantis_full <- c(1:total_sample)  
  mod_burnin <- fitstartyr*stepperyr+1
  fit_nyears <- fitendyr-fitstartyr
  fit_ntimes <- fit_nyears*stepperyr
  fittimes <- atlantis_full[mod_burnin:(mod_burnin+fit_ntimes-1)]
  fit_timesteps <- seq(fittimes[stepperyr], max(fittimes), by=stepperyr) #last timestep
  #fit_years <- unique(floor(fittimes/stepperyr)) #from Christine's new sardine_config.R
  fittimes.days <- if(omlist_ss$runpar$outputstepunit=="days") fittimes*omlist_ss$runpar$outputstep
  
  # catch files from CATCH.nc are subannual and by fleet
  # BUT have codes not species names,
  # and time in model timesteps not days
  # SO conversion needed
  
  # fishery cv lookup from config files
  fcvlook <- tibble::tibble()
  for(c in 1:length(fishcon)){
    source(fishcon[c], local = TRUE)
    fish_cv_n <- fish_cv %>%
      dplyr::mutate(fishery=fishery.name)
    fcvlook <- dplyr::bind_rows(fcvlook, fish_cv_n)
  }
  
  allcatch <- tibble::tibble()
  
  # limit catchbio_ss to names in fcvlook
  # WARNING this is now written only for output with Code and timestep output
  for(f in names(catchbio_ss)[names(catchbio_ss) %in% fcvlook$fishery]){
    catchbio <- catchbio_ss[[f]][[1]] %>%
      #dplyr::filter(time>0) %>%
      #dplyr::filter(time %in% fittimes.days) %>%
      #dplyr::mutate(year = time/365) %>%
      dplyr::filter(time %in% fittimes) %>%
      dplyr::mutate(year = ceiling(time/stepperyr),
                    fishMonth = 12 + ceiling(time/stepperyr*12) - year*12) %>%
      dplyr::select(species, year, fishMonth, atoutput) %>%
      dplyr::rename(catch = atoutput) %>%
      dplyr::left_join(dplyr::select(omlist_ss$funct.group_ss, Code, Name), by = c("species" = "Code")) %>%
      dplyr::mutate(ModSim = modsim) %>%
      dplyr::mutate(fishery = f) %>%
      #dplyr::mutate(area = 1) %>%
      dplyr::left_join(fcvlook) %>%
      dplyr::select(ModSim, year, fishMonth, Code=species, Name, fishery, everything()) %>%
      tidyr::pivot_longer(cols = c("catch", "cv"), 
                          names_to = "variable",
                          values_to = "value") %>%
      dplyr::mutate(units = ifelse(variable=="catch", "tons", "unitless")) %>%
      dplyr::arrange(Name, fishery, variable, year, fishMonth)
    
    allcatch <- dplyr::bind_rows(allcatch, catchbio)  
  }
  

  simCatchIndexSubannual <- allcatch
  
  #build new annual index from this one
  simCatchIndex <- mskeyrun::simCatchIndexSubannual %>%
    dplyr::group_by(ModSim, year, Code, Name, fishery, variable, units) %>%
    dplyr::summarize(value = sum(value, na.rm = TRUE)) %>%
    dplyr::mutate(value = ifelse(variable == "cv", value/stepperyr, value)) %>%
    dplyr::relocate(units, .after = last_col()) %>%
    dplyr::arrange(Name, fishery, variable, year)  
  
  
  
  if (saveToData) {
    usethis::use_data(simCatchIndexSubannual, overwrite = TRUE)
    usethis::use_data(simCatchIndex, overwrite = TRUE)
  }
  
  return(simCatchIndexSubannual)
  
  
}
