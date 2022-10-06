#' get numbers of sampled lengths for input into hydra
#' based on create_real_survey_lennumcomp.R
#' 

library(magrittr)

get_length_sample_n <- function(overwrite=T){
  
  survdatLWpull <- readRDS(here::here("data-raw/data/survdatLW.rds"))
  
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
    dplyr::filter(STRATUM %in% GBStrata) %>%
    dplyr::group_by(YEAR, SEASON, SVSPP) %>%
    dplyr::summarise(lensampsize = sum(NUMLEN, na.rm = TRUE),
                     ntows = length(unique(STATION))) %>%
    dplyr::left_join(species)
  
  surveyLenSampN <- newData
  
  if (overwrite) {
    usethis::use_data(surveyLenSampN,overwrite=overwrite)
  }
  
}

# # visualize
# library(ggplot2)
# ggplot2::ggplot(surveyLenSampN, aes(x=YEAR, y=lensampsize, colour = SEASON)) + 
#   geom_line()+ 
#   facet_wrap(~LongName, scales = "free_y")+
#   theme_bw()

# library(ggplot2)
# ggplot2::ggplot(surveyLenSampN, aes(x=YEAR, y=ntows, colour = SEASON)) + 
#   geom_line()+ 
#   facet_wrap(~LongName, scales = "free_y")+
#   theme_bw()
