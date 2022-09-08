#'Read in survey length weight data and create a table
#'
#'
#'

library(magrittr)
#channel <- dbutils::connect_to_database("sole","abeet")
source(here::here("data-raw/R/clean_lw_data.r"))


create_lw_table <- function(overwrite=F){

  # filter its
  itis <- mskeyrun::focalSpecies %>% 
    dplyr::select(SPECIES_ITIS,SVSPP,LongName) %>% 
    dplyr::filter(!(LongName == "Pollock")) %>%
    dplyr::distinct()
  
  lengthWeightAgeData <- clean_lw_data()
  
  # format
  lengthWeightAge <- lengthWeightAgeData %>%
    dplyr::mutate(YEAR = as.numeric(substring(CRUISE6,1,4)),
                  SVSPP = as.integer(SVSPP)) 
  
  # strata for Georges Bank
  GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
  plist <- list()
  paramTable <- NULL
  for (i in 1:length(itis$SVSPP)) {
    isp <- itis$SVSPP[i]
    speciesName <- itis$LongName[i]

    # filter by species and weight
    # may need to filter anomylous observations
    lengthWeight <- lengthWeightAge %>% 
      dplyr::filter(SVSPP==isp,
                    INDWT > 0) %>% 
      dplyr::mutate(INDWT = INDWT * 1000) %>% # convert to grams
      dplyr::mutate(SEX = as.factor(SEX)) 
    
    # GB stocks use length weight data from GB only
    if (any(isp %in% c(73,106,105))) { # cod, ytflounder wflounder
      lengthWeight %>% 
        dplyr::mutate(STRATUM = as.integer(STRATUM)) %>%
        dplyr::filter(STRATUM %in% GB) 
    }
    #dplyr::filter(LENGTH > 80 & INDWT < 3)
    
    # fit length weight relationships
    fits <- mscatch::fit_length_weight(lengthWeight,speciesName,speciesRules=NULL,suppressMessages=T)
    # estimated variance parameter
    sigma <- sqrt(fits$params$SINGLE$var)
    palpha <- as.numeric(exp(fits$params$SINGLE$logAlpha))
    pbeta <- as.numeric(fits$params$SINGLE$betas)
    
    # predicted weight
    lengthWeight$predWt <- exp(fits$model$SINGLE$fitted.values + (sigma^2)/2)
    
    # create list of plots
    p <- ggplot2::ggplot(data = lengthWeight, ggplot2::aes(x=LENGTH, y = INDWT, color = SEX)) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_line(ggplot2::aes(y = predWt),color = "red") +
      ggplot2::xlab("Length (cm)") +
      ggplot2::ylab("Weight (g)") +
      ggplot2::labs(title = paste0(speciesName))
    
    plist[[i]] <- list(p,speciesName)
    
    
    paramTable <- rbind(paramTable,data.frame(species=speciesName,
                                              alpha=palpha,
                                              beta=pbeta,
                                              sigma=sigma))
  }
  
  if(overwrite) {

    realBiolPar <- paramTable %>%
      dplyr::mutate(ModSim = "Actual") %>% 
      dplyr::left_join(mskeyrun::focalSpecies, by = c("species" = "LongName")) %>%
      dplyr::rename(Code = SPECIES_ITIS,
                    Name = modelName,
                    WLa = alpha,
                    WLb = beta) %>% 
      dplyr::select(ModSim,Code,Name,WLa,WLb,sigma) %>%
      dplyr::distinct() 
                
    
    usethis::use_data(realBiolPar,overwrite = overwrite)
    
    
  }
  return(list(paramTable=paramTable,plist=plist))
  
  
}