#' use survey data to compile length data for Hydra
#' 
#' Same format as mskeyrun:realFisheryLencomp
#' 
#' 
#' 
#' 

library(magrittr)


create_real_survey_lennumcomp <- function(convertKGtoMT=F, overwrite=F) {
  
  if (convertKGtoMT) {
    scalar <- 1000
    unitsLabel <- "metric tons"
  } else {
    scalar <- 1
    unitsLabel <- "kilograms"
  }

  # Pull data survey data with predicted weight at length based on LW relationships
  # and also the total hauled biomass regardless of length
  if(0) {
    channel <- dbutils::connect_to_database("sole","abeet")
    survdatLWpull <- survdat::get_survdat_data(channel = channel, filterByYear = 1968:2019, 
                                               all.season = F, shg.check = T, conversion.factor = T, use.SAD = F, 
                                               getBio = F, getLengths = T, getWeightLength = T)
    saveRDS(survdatLWpull,here::here("data-raw/data/survdatLW.rds"))
  } else { # read in saved file
    survdatLWpull <- readRDS(here::here("data-raw/data/survdatLW.rds"))
  }
    
  # filter GB region
  GBStrata  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)

  # get survey codes of focal species in mskeyrun
  species <- mskeyrun::focalSpecies %>% 
    dplyr::select(SVSPP,SPECIES_ITIS,LongName) %>% 
    dplyr::filter(SVSPP != 75) %>% #omit pollock
    dplyr::mutate(SPECIES_ITIS = as.double(SPECIES_ITIS)) %>%
    dplyr::distinct()
  
  # get survey codes
  svspps <- species$SVSPP
  
  newData <- survdatLWpull$survdat %>% 
    dplyr::filter(SVSPP %in% svspps) %>% 
    dplyr::mutate(SVSPPLEN = paste0(SVSPP,"-",LENGTH)) %>% 
    dplyr::select(-BIOMASS,-ABUNDANCE) %>%
    dplyr::mutate(BIOMASS = PREDWT,
                  ABUNDANCE = WGTLEN) 
  
  
  # fall: calculate the swept area biomass for each species length combination 
  sweptAreaFall <- survdat::calc_swept_area(newData,
                                            filterBySeason = "FALL",
                                            filterByArea = GBStrata,
                                            groupDescription = "SVSPPLEN") %>%
    dplyr::select(YEAR,SVSPPLEN,tot.biomass,tot.abundance) %>%
    tidyr::separate(.,SVSPPLEN,c("SVSPP","LENGTH"),sep = "-") %>% 
    dplyr::mutate(SEASON = "FALL")

  # spring: calculate the swept area biomass for each species length combination 
  sweptAreaSpring <- survdat::calc_swept_area(newData,
                                              filterBySeason = "SPRING",
                                              filterByArea = GBStrata,
                                              groupDescription = "SVSPPLEN") %>%
    dplyr::select(YEAR,SVSPPLEN,tot.biomass,tot.abundance) %>%
    tidyr::separate(.,SVSPPLEN,c("SVSPP","LENGTH"),sep = "-") %>% 
    dplyr::mutate(SEASON = "SPRING")

  # combine all data
  allData <- rbind(sweptAreaFall,sweptAreaSpring) %>%
    dplyr::filter(LENGTH != "NA") %>% 
    dplyr::mutate(SVSPP = as.integer(SVSPP),
                  LENGTH = as.double(LENGTH),
                  YEAR = as.integer(YEAR)) %>% 
    tibble::as_tibble()
  
  realSurveyLennumcomp <- allData %>%
    dplyr::left_join(.,species,by="SVSPP") %>%
    dplyr::mutate(ModSim = "Actual",
                  fishery = "demersal") %>%
    dplyr::rename(year = YEAR,
                  season = SEASON,
                  Code = SPECIES_ITIS,
                  Name = LongName,
                  lenbin = LENGTH,
                  numbers = tot.abundance) %>% 
    dplyr::mutate(biomass = tot.biomass/scalar) %>% # convert to metric tons)
    
    dplyr::group_by(ModSim,year,season,Code,Name,fishery,lenbin) %>%

    tidyr::pivot_longer(.,cols=c("biomass","numbers"),names_to = "variable",values_to = "value")  %>%
    dplyr::mutate(units = dplyr::case_when(variable == "biomass" ~ unitsLabel,
                                           variable == "numbers" ~ "numbers",
                                           TRUE ~ "NA")) %>%
    dplyr::select(ModSim,year,season, Code,Name, fishery, lenbin, variable, value, units) %>% 
    tibble::as_tibble(.)

  plistBiomassSpring <- list()
  plistBiomassFall <- list()
  plistAbundSpring <- list()
  plistAbundFall <- list()
  
  for (i in 1:length(svspps)) {
    isp <- svspps[i]

    targetSpecies <- species %>% 
      dplyr::filter(SVSPP == isp)
    nameSpecies <- targetSpecies$LongName
    itisSpecies <- targetSpecies$SPECIES_ITIS
    
    #Biomass
    seasons <- c("SPRING","FALL")
    for (iseason in 1:length(seasons)) {
      p <- realSurveyLennumcomp %>% 
        dplyr::filter(season == seasons[iseason],
                      Code == itisSpecies,
                      variable == "biomass") %>%
        ggplot2::ggplot() + 
        ggplot2::geom_col(mapping= ggplot2::aes(x=lenbin,y=value)) +
        ggplot2::facet_wrap(~year) +
        ggplot2::ylab(paste0("Biomass (",unitsLabel,")")) +
        ggplot2::ggtitle(nameSpecies)
      
      
      if (seasons[iseason] == "SPRING") {
        plistBiomassSpring[[i]] <- list(p,nameSpecies) 
      } else {
        plistBiomassFall[[i]] <- list(p,nameSpecies) 
      }
    }

    # Abundance
    for (iseason in 1:length(seasons)) {
      p <- realSurveyLennumcomp %>% 
        dplyr::filter(season == seasons[iseason],
                      Code == itisSpecies,
                      variable == "numbers") %>%
        ggplot2::ggplot() + 
        ggplot2::geom_col(mapping= ggplot2::aes(x=lenbin,y=value)) +
        ggplot2::facet_wrap(~year) +
        ggplot2::ylab(paste0("Abundance (numbers)")) +
        ggplot2::ggtitle(nameSpecies)
      
      
      if (seasons[iseason] == "SPRING") {
        plistAbundSpring[[i]] <- list(p,nameSpecies) 
      } else {
        plistAbundFall[[i]] <- list(p,nameSpecies) 
      }
    }
    
    
    
  }
  

  if (overwrite) {
    usethis::use_data(realSurveyLennumcomp,overwrite=overwrite)
  }
  
  
  return(list(data=realSurveyLennumcomp,
              plistBS=plistBiomassSpring,
              plistBF=plistBiomassFall,
              plistAS=plistAbundSpring,
              plistAF=plistAbundFall))
  
}








