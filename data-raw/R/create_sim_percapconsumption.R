#' Read in survey consumption and numbers data save as rda
#' 
#' atlantisom output is accessed and surveys pulled over time
#' 
#'@param atlmod configuration file specifying Atlantis simulation model filenames 
#'and locations  
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{year}{simulation year}
#'\item{Code}{Atlantis model three letter code for predator group}
#'\item{Name}{Atlantis model common name for predator group}
#'\item{agecl}{age class of Atlantis functional group}
#'\item{variable}{annual mean per capita consumption (intakeg)}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'

library(magrittr)

create_sim_percapconsumption <- function(atlmod,fitstart=NULL,fitend=NULL,saveToData=T) {

  # input is path to model config file for atlantisom
  source(atlmod)
  
  # path for survey and fishery config files
  cfgpath <- stringr::str_extract(atlmod, ".*config")
  
  #works because atlantis directory named for model and simulation
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modsim <- modpath[length(modpath)]
  
  #read in survey annual age comp data
  #all_diets <- atlantisom::read_savedsurvs(d.name, 'survDiet')
  
  # get config files -- needed?
  svcon <- list.files(path=cfgpath, pattern = "*survey*", full.names = TRUE)
  
  # omlist was amended with the new biomass_eaten generated in
  # https://github.com/ices-eg/wg_WGSAM/blob/master/SkillAssessment/SkillAssessProject.Rmd#L1116-L1198
  # see local file ~/Documents/0_Data/ms-keyrun/simulated-data/atlantisoutput/addcons_omlist.R
  
  # read true list with run and biol pars, etc
  omlist_ss <- readRDS(file.path(d.name, paste0(scenario.name, "omlist_ss.rds")))
  
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

  # need, total consumption per age class from atlantisom calc_pred_cons()
  # total n per age class
  # merge and leave either as total consumption or as intake (per capita consumption)
  # test functions for one species in NOBA
  
  truenumsatagecl <- omlist_ss$truenums_ss %>%
    dplyr::filter(time %in% fittimes) %>%
    dplyr::mutate(year = ceiling(time/stepperyr)) %>%
    dplyr::group_by(species, year, agecl) %>%
    dplyr::summarise(totNagecl = sum(atoutput)) #over layer and polygon
  
  intake <- omlist_ss$truecons_ss %>%
    dplyr::filter(time %in% fittimes) %>%
    dplyr::mutate(year = ceiling(time/stepperyr)) %>%
    dplyr::group_by(species, year, agecl) %>%
    dplyr::summarise(totconsagecl = sum(atoutput)) %>%
    dplyr::left_join(truenumsatagecl) %>%
    dplyr::mutate(intakeg = (totconsagecl/totNagecl)*1000000) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::select(omlist_ss$funct.group_ss, Code, Name), by = c("species" = "Name")) %>%
    dplyr::mutate(ModSim = modsim) %>%  
    dplyr::select(ModSim, year, Code, Name=species, agecl, intakeg) %>%
    tidyr::pivot_longer(cols = c("intakeg"), 
                        names_to = "variable",
                        values_to = "value") %>%
    dplyr::mutate(units = ifelse(variable=="intakeg", "grams per capita", "NA")) %>%
    dplyr::arrange(Name, variable, year)
  
  simPerCapCons <- intake
  
  if (saveToData) {
  
    usethis::use_data(simPerCapCons, overwrite = TRUE)
  }
  
  return(simPerCapCons)
  
  
}
