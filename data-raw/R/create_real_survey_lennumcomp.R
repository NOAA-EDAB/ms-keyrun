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
    survdatFull <- survdat::get_survdat_data(channel = channel, filterByYear = 1968:2019, 
                                             all.season = F, shg.check = T, conversion.factor = T, use.SAD = F, 
                                             getBio = F, getLengths = F, getWeightLength = F)
    
    saveRDS(survdatLWpull,here::here("data-raw/data/survdatLW.rds"))
    saveRDS(survdatFull,here::here("data-raw/data/survdatFull.rds"))
  } else { # read in saved file
    survdatLWpull <- readRDS(here::here("data-raw/data/survdatLW.rds"))
    survdatLFull <- readRDS(here::here("data-raw/data/survdatFull.rds"))
  }
  
  
  # get survey codes of focal species in mskeyrun
  species <- mskeyrun::focalSpecies %>% 
    dplyr::select(SVSPP,SPECIES_ITIS,LongName) %>% 
    dplyr::filter(SVSPP != 75) %>% #omit pollock
    dplyr::distinct()
  
  # get survey codes
  svspps <- species$SVSPP
  
  # aggregate sampled data to find total weight of subsampled species per tow
  # This is the total weight of all lengthed fish per tow
  sampledTow <- survdatLWpull$survdat %>% 
    dplyr::select(SVSPP,YEAR,SEASON,CRUISE6,STATION,STRATUM,TOW,LENGTH,NUMLEN,WGTLEN) %>%
    dplyr::filter(SVSPP %in% svspps,
                  LENGTH >0) %>%
    dplyr::arrange(SVSPP,YEAR,SEASON,CRUISE6,STATION,STRATUM,TOW) %>%
    assign("towlw",.,envir = .GlobalEnv) %>%
    dplyr::group_by(SVSPP,YEAR,SEASON,CRUISE6,STATION,STRATUM,TOW) %>% 
    dplyr::summarise(towSampleWeight = sum(WGTLEN),.groups="drop") %>%
    tibble::as_tibble()
  
  # aggregate the survey data without lengths for the total expanded biomass by tow
  fullTow <- survdatLFull$survdat %>%
    dplyr::select(SVSPP,YEAR,SEASON,CRUISE6,STATION,STRATUM,TOW,BIOMASS) %>%
    dplyr::filter(SVSPP %in% svspps) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(SVSPP,YEAR,SEASON,CRUISE6,STATION,STRATUM,TOW) %>% 
    dplyr::summarise(towWeight = sum(BIOMASS),.groups="drop") %>% 
    tibble::as_tibble() 
  
  
  # join the two tables
  # calculate the expansion factor from sampled weight to total survey weight
  db <- dplyr::left_join(fullTow,sampledTow,by = c("SVSPP", "YEAR", "SEASON", "CRUISE6", "STATION", "STRATUM", "TOW")) %>%
    dplyr::mutate(expansionFactor = towWeight/towSampleWeight)
  
  # apply this expansion factor to the weights of lengthed fish to "scale" up/down the biomass per length class
  # also apply expansion factor to numbers at length
  realSurveyLennumcomp <- dplyr::left_join(db,towlw,by = c("SVSPP", "YEAR", "SEASON", "CRUISE6", "STATION", "STRATUM", "TOW")) %>%
    dplyr::mutate(expNum = expansionFactor * NUMLEN) %>% 
    dplyr::mutate(weight = expansionFactor * WGTLEN) %>% 
    dplyr::left_join(.,species, by = "SVSPP") %>%
    dplyr::mutate(ModSim = "Actual",
                  fishery = "demersal") %>%
    dplyr::rename(year = YEAR,
                  season = SEASON,
                  Code = SPECIES_ITIS,
                  Name = LongName,
                  lenbin = LENGTH,
                  tbiomass = weight/dplyr::all_of(scalar), # convert to metric tons
                  tnumbers = expNum) %>% 

    dplyr::group_by(ModSim,year,season,Code,Name,fishery,lenbin) %>%
    dplyr::summarise(biomass = sum(tbiomass),
                     numbers = sum(tnumbers),.groups = "drop") %>%
    tidyr::pivot_longer(.,cols=c("biomass","numbers"),names_to = "variable",values_to = "value")  %>%
    dplyr::mutate(units = dplyr::case_when(variable == "biomass" ~ unitsLabel,
                                           variable == "numbers" ~ "numbers",
                                           TRUE ~ "NA")) 
    
    
  
  if (overwrite) {
    usethis::use_data(realSurveyLennumcomp,overwrite=overwrite)
  }
  
  
  return(realSurveyLennumcomp)
  
  
}
