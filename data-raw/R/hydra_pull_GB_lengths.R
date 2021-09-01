#' sample Data pull. All hydra species lengths
#'
#'Lengths pulled from "mv_cf_len" filtered to GB stat areas
#'
#'
#'@param channel obtained from logging into the sole database.
#'@param area stat areas to pull lengths from
#'


#channel <- cfdbs::connect_to_database("server","user") eventually remove this

hydra_pull_GB_lengths <- function(channel, area=c(cfdbs::EPUs$data$GB,537)) {
  library(magrittr)

  hydraSpecies <- data.frame(oldName=c("Acod","Aherring","Amackerel","goosefish","haddock",
                                       "silverhake","spinydog","winterfl","winterskate","yellowtailfl"),
                             newName=c("ATLANTIC COD","ATLANTIC HERRING","ATLANTIC MACKEREL","GOOSEFISH","HADDOCK",
                                       "SILVER HAKE","SPINY DOGFISH","WINTER FLOUNDER","WINTER SKATE","YELLOWTAIL FLOUNDER")) %>%
    dplyr::left_join(.,mscatch::speciesLookupTable,by=c("newName"="COMMON_NAME.y")) %>%
    dplyr::select(oldName,newName,SPECIES_ITIS) %>%
    dplyr::distinct()


  # pull sample length data and massage it
  message("Pulling length data ...")

  testDataPullLength <- cfdbs::get_landings_length(channel,year="all",area=area,species=hydraSpecies$SPECIES_ITIS,species_itis=T)

  # create unique tripid since NUMSAMP is replicated for each species reported within a trip
  lengths <- testDataPullLength$data %>%
    dplyr::mutate(tripid = paste0(PERMIT,YEAR,MONTH,DAY))

  # just extract the lengths and the number at length for the year, qr etc
  sampleLengths <- lengths %>%
    dplyr::select(YEAR,QTR,SPECIES_ITIS,NEGEAR,MARKET_CODE,LENGTH,NUMLEN,tripid) %>%
    dplyr::arrange(SPECIES_ITIS,YEAR,QTR,NEGEAR,MARKET_CODE,LENGTH)
  sampleLengths$SPECIES_ITIS <- as.integer(sampleLengths$SPECIES_ITIS)
  sampleLengths$YEAR <- as.integer(sampleLengths$YEAR)
  sampleLengths$QTR <- as.integer(sampleLengths$QTR)
  sampleLengths$LENGTH <- as.numeric(sampleLengths$LENGTH)
  sampleLengths$NUMLEN <- as.integer(sampleLengths$NUMLEN)

  sampleLengths <- dplyr::as_tibble(sampleLengths)

  saveRDS(sampleLengths,here::here("data-raw/data","GB_hydra_lengths.rds"))
  
  return(sampleLengths)

}
