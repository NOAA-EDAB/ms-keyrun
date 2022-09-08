#'Read in survey length weight data and clean it
#'
#' Remove anomolous values - manually
#' need a method to detect and remove
#'

 library(magrittr)
# channel <- dbutils::connect_to_database("sole","abeet")
# lengthWeightAgeData <- mscatch:::get_length_weight_age_data(channel,year="all", species=as.numeric(itis$SVSPP))
clean_lw_data <- function() {
  
  
  # read in data
  lengthWeightAgeData <- readRDS(here::here("data-raw/data/lengthWeigthAge.rds"))
  
  
  # filter itis
  itis <- mskeyrun::focalSpecies %>% 
    dplyr::select(SPECIES_ITIS,SVSPP,LongName) %>% 
    dplyr::filter(!(LongName == "Pollock")) %>%
    dplyr::distinct()
  
  
  # clean data for anomalous values
  # need a way to do this automatically
  
  
  lengthWeightAgeDat <- lengthWeightAgeData$data %>%
    dplyr::filter(!is.na(INDWT))
  lwaData <- NULL
  for (isvspp in itis$SVSPP) {
    
    if (isvspp == 197) {# goosefish
     newD <- lengthWeightAgeDat %>% 
        dplyr::filter(as.numeric(SVSPP) == 197)    %>%
        dplyr::filter(!(LENGTH > 80 & INDWT < 3))
    } else if (isvspp == 106) { # winterflounder
      newD <- lengthWeightAgeDat %>% 
        dplyr::filter(as.numeric(SVSPP) == 106)    %>%
        dplyr::filter(!(LENGTH < 20 & INDWT > 3))
    } else if (isvspp == 74) { # haddock
      newD <- lengthWeightAgeDat %>% 
        dplyr::filter(as.numeric(SVSPP) == 74)    %>%
        dplyr::filter(!(LENGTH > 60 & INDWT < 1))
    } else if (isvspp == 32) { # herring
      newD <- lengthWeightAgeDat %>% 
        dplyr::filter(as.numeric(SVSPP) == 32)    %>%
        dplyr::filter(INDWT < 4)
    } else if (isvspp == 15) { # dogfish
      newD <- lengthWeightAgeDat%>% 
        dplyr::filter(as.numeric(SVSPP) == 15)    %>%
        dplyr::filter(!(LENGTH > 75 & INDWT < 0.5))
    } else {
      newD <- lengthWeightAgeDat %>% 
        dplyr::filter(as.numeric(SVSPP) == isvspp)
    }
    
    lwaData <- rbind(lwaData,newD)
    
  }
  return(lwaData)
}