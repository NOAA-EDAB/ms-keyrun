#' sample Data pull
#'
#'Sample data in the format we'll need from proper data pulls using Seans comland script.
#'Landings pulled from "MV_CF_Landings", lengths pulled from "mv_cf_len"
#'
#'All missing areas need to dealt with prior to pulling species data.
#'Not the case in this sample. We aggregate all data as if from one EPU
#'
#'@param channel obtained from logging into the sole database.
#'@param species species_itis or nespp3 code. default = 164744 (147) (haddock - should be the easiest with most complete data)
#'@param species_itis. boolean. TRUE indicates species code is species_itis, FALSE = nespp3
#'
#'@section Other species of interest:
#'212 - atlantic mackerel - (K.Curti assessment)
#'081 - cod (mike palmer assessment)
#'

library(magrittr)
test_mscatch_data_pull <- function(channel,species=164744,species_itis = T,area="all",areaLengths="all", stock=NULL){ # species = 147


  ################ pull sample landings data and massage it
  #############################################################################################################
  message("Pulling landings data from STOCKEFF ...")
  testDataPullLandings <- cfdbs::get_landings(channel,year="all",area=area,species=species,species_itis=species_itis)

  ##############################
  ## All landings are in lbs ###
  ##############################

  ## for test data assume we this is EPU data. To achieve this we just sum over AREAS for now
  lands <- testDataPullLandings$data %>%
    dplyr::group_by(YEAR, MONTH, NEGEAR, MARKET_CODE) %>%
    dplyr::summarize(landings=sum(as.numeric(SPPLNDLB)),n=dplyr::n(),.groups="drop")
  lands <- dplyr::mutate(lands,QTR = as.character(ceiling(as.numeric(MONTH)/3 )))

  # aggregate landings by variables and count the number of trips
  sampleLandings <- lands %>%
    dplyr::group_by(YEAR,QTR,NEGEAR, MARKET_CODE) %>%
    dplyr::summarize(landings_land = sum(landings),landings_nn=sum(n),.groups="drop")
  # this needs to be checked.
  # filter all entries labelled quarter = 0
   sampleLandings <- sampleLandings %>%
    dplyr::select_all() %>%
    dplyr::filter(QTR != "0")

  #############################################################################################################
  # pull sample length data and massage it
   # option to pull lengths for a different spatial area
  message("Pulling length data ...")
  testDataPullLength <- cfdbs::get_landings_length(channel,year="all",area=areaLengths,species=species,species_itis=species_itis)
  # create unique tripid since NUMSAMP is replicated for each species reported within a trip
  lengths <- testDataPullLength$data %>%
    dplyr::mutate(tripid = paste0(PERMIT,YEAR,MONTH,DAY))
  # aggregate
  lengthsData <- lengths %>%
    dplyr::group_by(YEAR, QTR, NEGEAR, MARKET_CODE) %>%
    dplyr::summarize(len_totalNumLen=sum(as.numeric(NUMLEN)),len_numLengthSamples=length(unique(tripid)),.groups="drop")

  # full join of tables by common fields
  sampleData <- as.data.frame(dplyr::full_join(sampleLandings,lengthsData, by=c("YEAR","QTR","NEGEAR","MARKET_CODE")))
  sampleData$YEAR <- as.integer(sampleData$YEAR)
  sampleData$QTR <- as.integer(sampleData$QTR)
  # just extract the lengths and the number at length for the year, qr etc
  sampleLengths <- lengths %>% dplyr::select(YEAR,QTR,NEGEAR,MARKET_CODE,LENGTH,NUMLEN,tripid)
  sampleLengths$YEAR <- as.integer(sampleLengths$YEAR)
  sampleLengths$QTR <- as.integer(sampleLengths$QTR)
  sampleLengths$LENGTH <- as.numeric(sampleLengths$LENGTH)
  sampleLengths$NUMLEN <- as.integer(sampleLengths$NUMLEN)

  sampleLengths <- dplyr::as_tibble(sampleLengths)
  sampleData <- dplyr::as_tibble(sampleData)
  # save data


  if (is.null(stock)) {
    fileName <- species
  } else {
    fileName <- paste0(species,"_",stock)
  }

  vName <- paste0("sampleData_",fileName)
  assign(vName,sampleData)

  save(list=vName,file=paste0(here::here("data"),"/sampleData_",fileName,".rdata"))

  vName <- paste0("sampleLengths_",fileName)
  assign(vName,sampleLengths)

  #usethis::use_data(vName,overwite=T)
  save(list=vName,file=paste0(here::here("data"),"/sampleLengths_",fileName,".rdata"))

  #return(lengths)


}
