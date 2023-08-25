#' use survey data to compile length data for Hydra
#' 
#' Same format as mskeyrun:realFisheryLencomp
#' 
#' 
#' 
#' 

library(magrittr)
library(sf)

create_real_survey_lennumcomp <- function(convertKGtoMT=F,
                                          displayTable=F,
                                          overwrite=F) {
  
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
  
  # IS THIS CORRECT. WHY DID I USE WGTLEN AS ABUNDANCE
  # newData <- survdatLWpull$survdat %>% 
  #   dplyr::filter(SVSPP %in% svspps) %>% 
  #   dplyr::mutate(SVSPPLEN = paste0(SVSPP,"-",LENGTH)) %>% 
  #   dplyr::select(-BIOMASS,-ABUNDANCE) %>%
  #   dplyr::mutate(BIOMASS = PREDWT,
  #                 ABUNDANCE = WGTLEN)
  # 
  
  
  # I THINK THIS IS BETTER.
  # filter all GB Strata
  newData <- survdatLWpull$survdat %>% 
    dplyr::filter(STRATUM %in% GBStrata) %>% 
    dplyr::mutate(SVSPPLEN = paste0(SVSPP,"-",LENGTH))
  
  
  realSurveyLennumcomp <- NULL
  for (season in c("SPRING","FALL")) {
    # find number of tows in each stratum
    # table showing number of samples in each stratum
    longSampling <- newData %>%
      dplyr::filter(SEASON == season) %>%
      dplyr::select(YEAR,STRATUM,STATION,TOW) %>%
      dplyr::distinct() %>%
      dplyr::group_by(YEAR,STRATUM) %>%
      dplyr::summarise(ntows = dplyr::n(),.groups = "drop")

      showTable <- longSampling %>%
        tidyr::pivot_wider(.,id_cols = YEAR,names_from = STRATUM,values_from = ntows) %>% 
        flextable::flextable() %>%
        flextable::set_caption(paste("Number of Tows per STRATUM during ",season," Bottom Trawl Survey")) %>%
        flextable::colformat_double(.,big.mark = "",digits=0)
      if(displayTable) {
        print(showTable)
      }
    # plot GB strata
    nc <- NEFSCspatial::BTS_Strata %>%
      dplyr::filter(STRATA %in% unique(longSampling$STRATUM)) 
    GBsurveyPlot <- ggplot2::ggplot(data=nc ) +
      ggplot2::geom_sf() +
      ggplot2::geom_text(data=nc,ggplot2::aes(x=X,y=Y,label=STRATA),size=2) +
      ggplot2::ggtitle("Bottom Trawl Survey Strata")
    
    # calculate the area of each stratum in GB footprint
    strataAreas <- NEFSCspatial::BTS_Strata %>% 
      dplyr::filter(STRATA %in% unique(longSampling$STRATUM)) %>% 
      survdat::get_area(.,"STRATA") %>%
      dplyr::arrange(STRATUM)
    
    # aggregate data to calculate the expected number at length per tow
    # scale this up to stratum area based on area of average tow
    subsetData <- newData %>%
      dplyr::filter(SVSPP %in% svspps,
                    SEASON == season) %>%
      dplyr::select(YEAR,SVSPP,STRATUM,LENGTH,NUMLEN) %>%
      dplyr::filter(!STRATUM %in% c(3520,3550)) %>%
      dplyr::group_by(YEAR,SVSPP,STRATUM,LENGTH) %>% 
      dplyr::summarise(numlen = sum(NUMLEN),
                       .groups = "drop") %>% 
      dplyr::left_join(.,longSampling, by = c("YEAR","STRATUM")) %>%
      dplyr::left_join(.,strataAreas,by = "STRATUM") %>%
      dplyr::mutate(expNUMpertow = numlen/ntows,
                    expNUMperstratum = as.numeric(expNUMpertow*Area/.0384)) 
    
    # format aggregated data to format we require
    out <- subsetData %>%
      dplyr::group_by(YEAR,SVSPP,LENGTH) %>%
      dplyr::summarise(value = sum(expNUMperstratum),
                       .groups = "drop") %>%
      dplyr::left_join(.,species, by = "SVSPP") %>%
      dplyr::mutate(year = as.integer(YEAR),
                    season = season,
                    lenbin = LENGTH,
                    Code = SPECIES_ITIS,
                    Name = LongName,
                    units = "numbers",
                    variable = "numbers",
                    ModSim = "Actual",
                    fishery="demersal") %>%
      dplyr::select(ModSim,year,season,Code,Name,fishery,lenbin,variable,value,units)
  
    realSurveyLennumcomp <- rbind(realSurveyLennumcomp,out)
    
  }
  
  realSurveyLennumcomp <- realSurveyLennumcomp %>%
    dplyr::filter(!is.na(lenbin))
  
# 
#   sweptAreaFall <- survdat::calc_swept_area(newData,
#                                             filterBySeason = "FALL",
#                                             filterByArea = GBStrata,
#                                             groupDescription = "SVSPP") %>%
#     dplyr::select(YEAR,SVSPP,tot.biomass,tot.abundance) %>%
#     dplyr::mutate(SEASON = "FALL")
#   # fall: calculate the swept area biomass for each species length combination 
#   sweptAreaFall <- survdat::calc_swept_area(newData,
#                                             filterBySeason = "FALL",
#                                             filterByArea = GBStrata,
#                                             groupDescription = "SVSPPLEN") %>%
#     dplyr::select(YEAR,SVSPPLEN,tot.biomass,tot.abundance) %>%
#     tidyr::separate(.,SVSPPLEN,c("SVSPP","LENGTH"),sep = "-") %>% 
#     dplyr::mutate(SEASON = "FALL")
#   message("FALL")
# 
#   # spring: calculate the swept area biomass for each species length combination 
#   sweptAreaSpring <- survdat::calc_swept_area(newData,
#                                               filterBySeason = "SPRING",
#                                               filterByArea = GBStrata,
#                                               groupDescription = "SVSPPLEN") %>%
#     dplyr::select(YEAR,SVSPPLEN,tot.biomass,tot.abundance) %>%
#     tidyr::separate(.,SVSPPLEN,c("SVSPP","LENGTH"),sep = "-") %>% 
#     dplyr::mutate(SEASON = "SPRING")
#   message("SPRING")
#   # combine all data
#   allData <- rbind(sweptAreaFall,sweptAreaSpring) %>%
#     dplyr::filter(LENGTH != "NA") %>% 
#     dplyr::mutate(SVSPP = as.integer(SVSPP),
#                   LENGTH = as.double(LENGTH),
#                   YEAR = as.integer(YEAR)) %>% 
#     tibble::as_tibble()
#   
#   realSurveyLennumcomp <- allData %>%
#     dplyr::left_join(.,species,by="SVSPP") %>%
#     dplyr::mutate(ModSim = "Actual",
#                   fishery = "demersal") %>%
#     dplyr::rename(year = YEAR,
#                   season = SEASON,
#                   Code = SPECIES_ITIS,
#                   Name = LongName,
#                   lenbin = LENGTH,
#                   numbers = tot.abundance) %>% 
#     dplyr::mutate(biomass = tot.biomass/scalar) %>% # convert to metric tons)
#     
#     dplyr::group_by(ModSim,year,season,Code,Name,fishery,lenbin) %>%
# 
#     tidyr::pivot_longer(.,cols=c("biomass","numbers"),names_to = "variable",values_to = "value")  %>%
#     dplyr::mutate(units = dplyr::case_when(variable == "biomass" ~ unitsLabel,
#                                            variable == "numbers" ~ "numbers",
#                                            TRUE ~ "NA")) %>%
#     dplyr::select(ModSim,year,season, Code,Name, fishery, lenbin, variable, value, units) %>% 
#     tibble::as_tibble(.)

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
              #plistBS=plistBiomassSpring,
              #plistBF=plistBiomassFall,
              plistAS=plistAbundSpring,
              plistAF=plistAbundFall))
  
}








