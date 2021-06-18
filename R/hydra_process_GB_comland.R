#' sample Data pull of all species in Hydra model on GB
#'
#'Sample data in the format we'll need from proper data pulls using Seans comland script.
#'Landings pulled using comlandr
#'
#'All missing areas need to dealt with prior to pulling species data.
#'Not the case in this sample. We pull from GB
#'
#'@param channel obtained from logging into the sole database.
#'@param comlandData data from comland pull
#'
#'

#channel <- cfdbs::connect_to_database("server","user")
# need to pull comland data. Missing MARKET_CODE because pull from AA not stockeff
# comlandData <- readRDS(here::here("data-raw","comland_negear.rds"))
# lengthData <- readRDS(here::here("data-raw","hydra_lengths.rds")) # see test_data_pull_hydra_GB_lengths.r
# library(magrittr)

process_hydra_GB_comland <- function(channel,comlandData,lengthData){

  hydraSpecies <- data.frame(oldName=c("Acod","Aherring","Amackerel","goosefish","haddock",
                                       "silverhake","spinydog","winterfl","winterskate","yellowtailfl"),
                             newName=c("ATLANTIC COD","ATLANTIC HERRING","ATLANTIC MACKEREL","GOOSEFISH","HADDOCK",
                                       "SILVER HAKE","SPINY DOGFISH","WINTER FLOUNDER","WINTER SKATE","YELLOWTAIL FLOUNDER")) %>%
    dplyr::left_join(.,mscatch::speciesLookupTable,by=c("newName"="COMMON_NAME.y")) %>%
    dplyr::select(oldName,newName,SPECIES_ITIS,SVSPP) %>%
    dplyr::distinct()

  GBArea <- c(cfdbs::EPUs$data$GB,537)

  ## for test data assume we this is EPU data. To achieve this we just sum over AREAS for now
  lands <- comlandData$comland %>%
    dplyr::filter(AREA %in% GBArea) %>%
    #dplyr::rename(MARKET_CODE = MKTCAT) %>%
    dplyr::group_by(YEAR, MONTH, NEGEAR, NESPP3) %>%
    #dplyr::mutate(SPPLNDLB = SPPLIVMT/0.00045359237)  %>%
    dplyr::summarize(landings=sum(as.numeric(SPPLIVMT)),n=dplyr::n(),.groups="drop")
  lands <- dplyr::mutate(lands,QTR = as.character(ceiling(as.numeric(MONTH)/3 )))

  # aggregate landings by variables and count the number of trips
  sampleLandings <- lands %>%
    dplyr::group_by(YEAR,QTR,NEGEAR,NESPP3) %>%
    dplyr::summarize(landings_land = sum(landings),landings_nn=sum(n),.groups="drop")
  # this needs to be checked.
  # filter all entries labeled quarter = 0
  sampleLandings <- sampleLandings %>%
    dplyr::select_all() %>%
    dplyr::filter(QTR != "0")

  # filter lookuptable
  lookup <- mscatch::speciesLookupTable %>%
    dplyr::select(SPECIES_ITIS,NESPP3) %>%
    dplyr::mutate(SPECIES_ITIS = as.numeric(SPECIES_ITIS),NESPP3=as.numeric(NESPP3))

  # filter hydra species
  sampleLandings <- sampleLandings %>%
    dplyr::filter(NESPP3 %in% lookup$NESPP3) %>%
    dplyr::left_join(.,lookup,by="NESPP3") %>%
    dplyr::select(-NESPP3) %>%
    dplyr::mutate(SPECIES_ITIS = as.integer(SPECIES_ITIS)) %>%
    dplyr::mutate(QTR = as.integer(QTR)) %>%
    dplyr::mutate(YEAR = as.integer(YEAR)) %>%
    dplyr::mutate(NEGEAR = sprintf("%03d",NEGEAR))

  sampleLengths <- lengthData %>%
    dplyr::group_by(SPECIES_ITIS,YEAR,QTR,NEGEAR,LENGTH,tripid) %>%
    dplyr::summarize(NUMLEN = sum(NUMLEN),.groups = "drop") # since we dont have/need MARKET_CODES sum over market CODE data

  lengthsData <- sampleLengths %>%
    dplyr::group_by(SPECIES_ITIS, YEAR, QTR, NEGEAR) %>%
    dplyr::summarize(len_totalNumLen=sum(as.numeric(NUMLEN)),len_numLengthSamples=length(unique(tripid)),.groups="drop")

  sampleData <- sampleLandings %>%
    dplyr::left_join(.,lengthsData, by=c("SPECIES_ITIS","YEAR","QTR","NEGEAR"))

  itisCodes <- unique(hydraSpecies$SPECIES_ITIS)
  expanded <- list()
  # loop over species and expand landings by length
  for (itis in itisCodes) {
    message(paste0("working on : ",itis))
    species <- hydraSpecies %>%
      dplyr::filter(SPECIES_ITIS == itis)

    fileName <- paste0(itis,"_GB")
    landings <- sampleData %>%
      dplyr::filter(SPECIES_ITIS == itis)
    lengths <- sampleLengths %>%
      dplyr::filter(SPECIES_ITIS == itis)

    # cleans landings data and length data of NAs
    landingsData <- landings %>% dplyr::group_by(YEAR,QTR,NEGEAR) %>%
      dplyr::summarise(landings_land=sum(landings_land, na.rm=T),
                       landings_nn=sum(landings_nn, na.rm=T),
                       len_totalNumLen=sum(len_totalNumLen,na.rm=T),
                       len_numLengthSamples=sum(len_numLengthSamples, na.rm=T),
                       .groups="drop")
    lengthData <- lengths %>% dplyr::group_by(YEAR,QTR,NEGEAR,LENGTH) %>%
      dplyr::summarise(NUMLEN = sum(as.numeric(NUMLEN),na.rm=T),.groups="drop")


    annualLandings <- landingsData %>%
      dplyr::group_by(YEAR, NEGEAR) %>%
      dplyr::summarise(landings_land = sum(landings_land),
                       len_totalNumLen = sum(len_totalNumLen),
                       len_numLengthSamples = sum(len_numLengthSamples),
                       landings_nn = sum(landings_nn),.groups="drop")

    annualLengths <- lengthData %>%
      dplyr::group_by(YEAR, NEGEAR,LENGTH) %>%
      dplyr::summarise(NUMLEN = sum(NUMLEN),.groups="drop")

    lengthWeightData <- svdbs::get_length_weight(channel,year="all", species=as.numeric(species$SVSPP))
    # filter outliers
    #message(paste0("Largest Fish by weight = ", max(lengthWeightData$data$INDWT,na.rm = T)))
    #next
    # 5. create length weight
    fits <- mscatch::fit_length_weight(lengthWeightData$data,species$newName ,here::here(),logfile="logfile.txt")

    nParams <- length(fits$commonSlope$coefficients)
    lengthWeightParams <- list()
    lengthWeightParams$alpha <- fits$commonSlope$coefficients[1]
    lengthWeightParams$betas <- fits$commonSlope$coefficients[2:nParams]
    lengthWeightParams$var <- sum(fits$commonSlope$residuals^2)/fits$commonSlope$df.residual

    # 6. # apply equations to lengthData and landingsData to expand
    expandedLandings <- mscatch::expand_landings_to_lengths(annualLandings,annualLengths,lengthWeightParams) %>%
      dplyr::mutate(species_itis = itis)

    expanded <- rbind(expanded,expandedLandings)


  }

  return(expanded)
  #saveRDS(expanded,here::here("data-raw","expandedHydraLandings.rds"))
}
