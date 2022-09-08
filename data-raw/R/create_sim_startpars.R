#' Read in initial age and recruitment and save as rda
#' 
#' atlantosom output is accessed and true values pulled for par inputs
#' 
#'@param atlmod configuration file specifying Atlantis simulation model filenames 
#'and locations  
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{Code}{Atlantis model three letter code for functional group}
#'\item{Name}{Atlantis model common name for functional group}
#'\item{agecl}{age class or true age of Atlantis functional group}
#'\item{lenbin}{1 cm length bin, lower limit}
#'\item{variable}{start year number at age class and length (Natlen), start year number at true age (Natage), average recruitment (AvgRec)}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'

library(magrittr)

create_sim_startpars <- function(atlmod,fitstart=NULL,fitend=NULL,saveToData=T) {

  # input is path to model config file for atlantisom
  source(atlmod)
  
  # path for survey and fishery config files
  cfgpath <- stringr::str_extract(atlmod, ".*config")
  
  #works because atlantis directory named for model and simulation
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modsim <- modpath[length(modpath)]
  
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
  
  
  # true N at agecl at model fit start, can run through age2length
  # keep first 5 timesteps (1 yr) to filter later for time when each species recruits
  modstartN <- omlist_ss$truenums_ss %>%
    dplyr::filter(time %in% fittimes[1:5]) %>%
    # group_by(species) %>%
    # filter(time == min(time)) %>%
    dplyr::group_by(species, agecl, time) %>%
    dplyr::summarise(atoutput = sum(atoutput)) %>%
    dplyr::mutate(polygon = "NA",
           layer = "NA") %>%
    as.data.frame()
  
  modstartresn <- omlist_ss$trueresn_ss %>%
    dplyr::filter(time %in% fittimes[1:5]) %>%
    #group_by(species) %>%
    #filter(time == min(time)) %>%
    dplyr::group_by(species, agecl, time) %>%
    dplyr::summarise(atoutput = median(atoutput)) %>%
    dplyr::mutate(polygon = "NA",
           layer = "NA") %>%
    as.data.frame()
  
  modstartstructn <- omlist_ss$truestructn_ss %>%
    dplyr::filter(time %in% fittimes[1:5]) %>%
    #group_by(species) %>%
    #filter(time == min(time)) %>%
    dplyr::group_by(species, agecl, time) %>%
    dplyr::summarise(atoutput = median(atoutput)) %>%
    dplyr::mutate(polygon = "NA",
           layer = "NA") %>%
    as.data.frame()
  
  # both below from survey configs
  lenage_cv <- 0.1
  
  # max size bin for length estimation, function defaults to 150 cm if not supplied
  maxbin <- 200
  
  modstartlen <- atlantisom::calc_age2length(structn = modstartstructn,
                                             resn = modstartresn,
                                             nums = modstartN,
                                             biolprm = omlist_ss$biol,
                                             fgs = omlist_ss$funct.group_ss,
                                             maxbin = maxbin,
                                             CVlenage = lenage_cv,
                                             remove.zeroes=TRUE)
  
  #find the time with the smallest fish (recruitment!) and use that for each spp
  smallest <- modstartlen$natlength %>%
    dplyr::group_by(species) %>%
    dplyr::filter(lower.bins == min(lower.bins)) %>%
    dplyr::filter(atoutput == max(atoutput)) %>%
    dplyr::select(species, rectime=time)
  
  
  modstartNlen <- modstartlen$natlength %>%
    dplyr::select(species, agecl, time, lower.bins, atoutput) %>%
    dplyr::left_join(smallest) %>%
    dplyr::filter(time == rectime) %>%
    dplyr::select(-time, -rectime) %>%
    dplyr::left_join(dplyr::select(omlist_ss$funct.group_ss, Code, Name), by = c("species" = "Name")) %>%
    dplyr::mutate(ModSim = modsim) %>%
    dplyr::select(ModSim, Code, Name=species, agecl, lenbin=lower.bins, Natlen = atoutput) %>%
    tidyr::pivot_longer(cols = c("Natlen"),
                        names_to = "variable",
                        values_to = "value") %>%
    dplyr::mutate(units = ifelse(variable=="Natlen", "number at length in agecl in start year", "NA")) %>%
    dplyr::arrange(Name, variable,agecl, lenbin)
  
  #use same smallest fish time for numbers at true age
  modstartNage <- omlist_ss$truenumsage_ss %>%
    dplyr::filter(time %in% fittimes[1:5]) %>%
    dplyr::group_by(species, agecl, time) %>%
    dplyr::summarise(atoutput = sum(atoutput)) %>%
    dplyr::left_join(smallest) %>%
    dplyr::filter(time == rectime) %>%
    dplyr::select(-time, -rectime) %>%
    dplyr::left_join(dplyr::select(omlist_ss$funct.group_ss, Code, Name), by = c("species" = "Name")) %>%
    dplyr::mutate(ModSim = modsim) %>%
    dplyr::mutate(lenbin = NA) %>%  
    dplyr::select(ModSim, Code, Name=species, agecl, lenbin, Natage = atoutput) %>%
    tidyr::pivot_longer(cols = c("Natage"),
                        names_to = "variable",
                        values_to = "value") %>%
    dplyr::mutate(units = ifelse(variable=="Natage", "number at true age in start year", "NA")) %>%
    dplyr::arrange(Name, variable,agecl)
  

  
  # get average recruitment
  nitro <- merge(omlist_ss$biol$kwsr, omlist_ss$biol$kwrr, by = "1")
  names(nitro) <- c("Code", "KWSR", "KWRR")
  
  nitro <- nitro %>%
    dplyr::group_by(Code) %>%
    dplyr::mutate(Nsum = sum(KWSR, KWRR))
  
  truerec <- omlist_ss$YOY_ss %>% 
    tidyr::pivot_longer(-Time, names_to = "Code", values_to = "recwt") %>%
    dplyr::mutate(Code = gsub("\\.0", "", Code)) %>%
    dplyr::filter(Time>0) %>% # this output not meaningful
    dplyr::filter(Time %in% seq(365, max(Time), by=365)) %>%
    dplyr::left_join(omlist_ss$funct.group_ss[, c("Code", "Name")]) %>%
    dplyr::left_join(nitro) %>%
    dplyr::mutate(recnums = (recwt * 50000000.0 / omlist_ss$biol$redfieldcn) / Nsum) %>%
    dplyr::group_by(Name) %>%
    dplyr::mutate(meanrecnums = mean(recnums))
  
  #model input log(millions)
  avgrec <- truerec %>%
    dplyr::filter(Time %in% fittimes.days) %>%
    dplyr::group_by(Name) %>%
    dplyr::summarize(AvgRec = mean(log(recnums/1000000))) %>%
    dplyr::ungroup() 
  
  # some rec missing from YOY file so sub in age 1s as start values
  #sub these in if above is -Inf, scale will be off but starting value
  trueage1 <- omlist_ss$truenumsage_ss %>%
    dplyr::filter(agecl==1) %>%
    dplyr::group_by(species, agecl, time) %>%
    dplyr::summarise(age1N = sum(atoutput)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = ceiling(time/stepperyr)) %>%
    dplyr::group_by(species, agecl, year) %>%
    dplyr::filter(age1N == max(age1N)) %>%
    dplyr::select(Name=species, Time=time, age1N, year) %>%
    dplyr::group_by(Name) %>%
    dplyr::mutate(meanage1N = mean(age1N))
  
  avgage1N <- trueage1 %>%
    dplyr::filter(Time %in% fittimes) %>%
    dplyr::group_by(Name) %>%
    dplyr::summarize(Avgage1N = mean(log(age1N/1000000))) %>%
    dplyr::ungroup() 
  
  #scale up avgage 1 by mean ratio across stocks? 
  meanavgrec <- avgrec %>%
    dplyr::filter(is.finite(AvgRec)) %>%
    dplyr::summarise(mean(AvgRec))
  
  meanavgage1 <- mean(avgage1N$Avgage1N)
  
  scale <- as.numeric(meanavgrec/meanavgage1)
  
  avgrec1 <- merge(avgrec, avgage1N) %>%
    dplyr::mutate(scalerec = Avgage1N * scale) %>%
    dplyr::filter(is.infinite(AvgRec)) %>%
    dplyr::select(Name, AvgRec=scalerec)
  
  if(length(avgrec1)>0){
    avgrec <- avgrec %>%
      dplyr::filter(is.finite(AvgRec)) %>%
      dplyr::full_join(avgrec1) %>%
      dplyr::arrange(Name)
  }
  
  avgrec <- avgrec %>%
    dplyr::left_join(dplyr::select(omlist_ss$funct.group_ss, Code, Name)) %>%
    dplyr::mutate(ModSim = modsim) %>%
    dplyr::mutate(agecl = NA) %>% 
    dplyr::mutate(lenbin = NA) %>%  
    dplyr::select(ModSim, Code, Name, agecl, lenbin, AvgRec) %>%
    tidyr::pivot_longer(cols = c("AvgRec"),
                        names_to = "variable",
                        values_to = "value") %>%
    dplyr::mutate(units = ifelse(variable=="AvgRec", "log average recruitment, millions", "NA")) %>%
    dplyr::arrange(Name, variable,agecl)
  
  
  simStartPars <- dplyr::bind_rows(modstartNlen, modstartNage, avgrec)
  
  if (saveToData) {
  
    usethis::use_data(simStartPars, overwrite = TRUE)
  }
  
  return(simStartPars)
  
  
}
