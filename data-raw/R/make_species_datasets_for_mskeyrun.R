#' Creates the data sets for mskeyrun.
#'
#' All species have a fixed time interval, QTR, SEMESTER, YEAR.
#' All species borrow samples.
#' UNclassified use the same temporal aggregation
#' All species use length data from survey to fit length weight relationships
#' Landings from GB statistical areas
#' Lengths from same area unless shelf wide stock, then lengths from larger area
#'
#' This alleviates the immediate need for coding the length expansion to deal
#' with differing levels of aggregation that would arise from collapsing time
#' based on sample availability
#'
#'

#channel <- dbutils::connect_to_database("sole","abeet")

library(magrittr)

make_species_datasets_for_mskeyrun <- function(channel) {


  itis <- mscatch::speciesLookupTable %>%
    dplyr::distinct(SPECIES_ITIS) %>%
    dplyr::filter(!(SPECIES_ITIS %in% c(164727,166774))) %>%
    dplyr::pull()


  for (species_itis in itis) {

    speciesRules <- mscatch::get_species_object_mskeyrun(species_itis)

    message(paste0("Processing ",speciesRules$speciesName))
    #speciesRules$howAggregate <- "borrow"
    #speciesRules$LengthWeightData <- "survey"

    aggregate_to = toupper(speciesRules$temporalAggregation)
    stock <- "GB"#speciesRules$stock
    borrowLengths <- T

    speciesName <- speciesRules$speciesName
    speciessvspp <- speciesRules$SVSPP

    outputDir <- here::here("output",paste0(speciesName[1],"_",stock,"_tt_",aggregate_to,"_",speciesRules$howAggregate))

    if (!dir.exists(outputDir)){ # create directory to store exploratory/informational plots
      dir.create(outputDir)
    }

    # landings from GB, lengths from NEUS wide
    # All landings are in lbs (from stockeff)
    landName <- rlang::parse_expr(paste0("mscatch::sampleData_",species_itis,"_GB"))
    lengthName <- rlang::parse_expr(paste0("mscatch::sampleLengths_",species_itis,"_GB"))

    # filter lengths based on spurious values. Will eventually do this during data pull
    if (species_itis == 160617) {
     lengthData <- eval(lengthName) %>% dplyr::filter(LENGTH < 700)
    } else if (species_itis %in% c(172414,164791)) {
      lengthData <- eval(lengthName) %>% dplyr::filter(LENGTH < 100)
    } else if (species_itis == 164712) {
      lengthData <- eval(lengthName) %>% dplyr::filter(LENGTH < 400)
    } else if (species_itis == 164499) { # goosefish 1 large individual
      lengthData <- eval(lengthName) %>% dplyr::filter(LENGTH < 150)
    # } else if (species_itis == 161722) {
    #     lengthData <- eval(lengthName) %>% dplyr::filter(LENGTH < 50)
    } else {
      lengthData <- eval(lengthName)
    }

    # aggregate landings
    # result is in lbs
    aggData <- mscatch::aggregate_landings(channel=channel,
                                           landingsData=eval(landName),
                                           lengthData=lengthData ,
                                           outputDir=outputDir,
                                           landingsThresholdGear = .95,
                                           speciesName = NULL,
                                           aggregate_to = aggregate_to,
                                           borrowLengths = borrowLengths,
                                           outputPlots = T,
                                           speciesRules=speciesRules,
                                           pValue = .999)

    message("getting length weight data")
    # grab length-weight-age data from survey cruises
    if (speciesRules$LengthWeightData == "survey") {
      # Pulling form FSCS bio. indwt is in kg
      lengthWeightAgeData <- mscatch:::get_length_weight_age_data(channel,year="all", species=as.numeric(speciessvspp))
      #lengthWeightAgeData <- readRDS(here::here("data-raw/data/",paste0("lengthWeight",as.numeric(speciessvspp),".rds")))
      saveRDS(lengthWeightAgeData,here::here("data-raw/data",paste0("surveyLengthWeightAge",species_itis,".rds")))

      #lengthWeightAgeData <- readRDS(here::here("data-raw/data/",paste0("surveyLengthWeightAge",species_itis,".rds")))
    } else if (speciesRules$LengthWeightData == "commercial") {

    } else {

    }
    # format
    lengthWeightAge <- lengthWeightAgeData$data %>%
      dplyr::mutate(YEAR = as.numeric(substring(CRUISE6,1,4)))

    # create length weight
    message("Fitting length- weight models")
    message(paste0("Processing ",speciesRules$speciesName))

    # fit in (kg)
    fits <- mscatch::fit_length_weight(lengthWeightAge,speciesName,speciesRules,outputDir,logfile="logfile.txt")


    message("Expanding landings to lengths")
    # must expand landings from lbs to kg since fit is in kg
    lbTokg <- 2.2046226218 #(1kg = 2.2046226218 lbs)
    aggData$landings %>%
      dplyr::mutate(landings_land = landings_land/lbTokg)

    expandedLandings <- mscatch::expand_landings_to_lengths(aggData$landings,aggData$lengthData,fits$params) %>%
      dplyr::mutate(species_itis = species_itis)

    saveRDS(expandedLandings,here::here("data-raw/data",paste0("expandedLandings",species_itis,".rds")))

    ##### lengths FINISHED here

    ## Age data

    if (any(is.na(speciesRules$AgeLengthKey))) {
      # spiny dog and winter skate. No age structure
      next
    }

    # channel <- dbutils:: connect_to_database("sole","abeet")
    agesFromComland <- cfdbs::get_age_length(channel, year = "all",species = species_itis,species_itis = T)$data %>%
      dplyr::mutate(YEAR = as.double(YEAR))
    saveRDS(agesFromComland,here::here("data-raw/data",paste0("age",species_itis,".rds")))
#    ageComland <- readRDS(here::here("data-raw/data",paste0("age",species_itis,".rds")))

    # format to survey data to match commercial for combining
    lengthWeightAgeSurvey <- lengthWeightAgeData$data %>%
      dplyr::mutate(YEAR = as.numeric(substring(CRUISE6,1,4))) %>%
      dplyr::mutate(NUMAGE = 1) %>%
      dplyr::select(YEAR,SEASON,SEX,AGE,INDWT,LENGTH,NUMAGE)

    # combine all age data from survey and commercial
    ageData <- mscatch::combine_age_data(lengthWeightAgeSurvey,agesFromComland,aggregate_to)

    # find age length keys by year/semester
    ageLengthKeys <- mscatch::create_yr_semester_age_length_key(expandedLandings,
                                                                ageData,
                                                                filterYrs = min(ageData$YEAR),
                                                                plusAge = speciesRules$maxAge,
                                                                referenceAge = 2)

    # use Age length key to calculate numbers at age in each semester
    exland <- mscatch::calc_numbers_at_age(expandedLandings,ageLengthKeys$alKey,fits$params)

    saveRDS(exland,here::here("data-raw/data",paste0("numbersatAge",species_itis,".rds")))

  }
}


# then run
#create_real_fishery_lencomp()
#create_real_fishery_agecomp()

