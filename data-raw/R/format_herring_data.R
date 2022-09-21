#' reads in length data for herring
#'
#' Data pulled from maine data tables. use all lengths and apply
#' to GB landings since a single stock
#'
#' Need to convert MT (herring database) to pounds (comlandr)
#' Need to convert mm (herring lengths) to cm (svdbs)
#'

#data <- survdat::get_length_weight_data()
library(magrittr)


format_herring_data <- function(overwrite=F) {

  kgTolbs <- 2.2046226218 #(1kg = 2.2046226218 lbs)

  #herring data
  herring <- readRDS(here::here("data-raw/data","herringData.rds"))

  # length and weight data
  lengthWeightData <- herring$length %>%
    dplyr::mutate(tripid = "maine") %>%
    dplyr::mutate(MARKET_CODE = "UN") %>%
    dplyr::mutate(YEAR = as.integer(YEAR),
                  MONTH = as.integer(MONTH)) %>%
    dplyr::mutate(QTR = dplyr::case_when(MONTH %in% c(1:3) ~ 1,
                                         MONTH %in% c(4:6) ~ 2 ,
                                         MONTH %in% c(7:9) ~ 3,
                                         MONTH %in% c(10:12) ~ 4,
                                         TRUE ~ 0)) %>%
    dplyr::mutate(LENGTH = round(SAMPLENGTH/10)) # convert mm to cm

  # format for mscatch
  lengthData <- lengthWeightData %>%
    dplyr::select(YEAR,QTR,NEGEAR,MARKET_CODE,LENGTH,tripid) %>%
    dplyr::group_by(YEAR,NEGEAR,QTR,MARKET_CODE,LENGTH,tripid) %>%
    dplyr::summarise(NUMLEN = dplyr::n(),.groups="drop") %>%
    dplyr::mutate(QTR = as.integer(QTR)) %>%
    dplyr::filter(!is.na(LENGTH)) %>%
    dplyr::select(YEAR,QTR,NEGEAR,MARKET_CODE,LENGTH,NUMLEN,tripid)


  lengthDataSum <- lengthData %>%
    dplyr::group_by(YEAR, QTR, NEGEAR, MARKET_CODE) %>%
    dplyr::summarize(len_totalNumLen=sum(as.numeric(NUMLEN)),len_numLengthSamples=length(unique(tripid)),.groups="drop")

  # landings data from comlandr for GB region only
  sampleLandings <- herring$catch %>%
    dplyr::mutate(AREA = as.integer(STOCK_AREA)) %>%
    dplyr::filter(AREA %in% c(521,522,523,524,525,526,538,551,552,561,562,537)) %>%
    dplyr::mutate(YEAR = as.integer(YEAR)) %>%
    dplyr::mutate(MONTH = as.integer(MONTH)) %>%
    dplyr::mutate(QTR = dplyr::case_when(MONTH %in% c(1:3) ~ as.integer(1),
                                         MONTH %in% c(4:6) ~ as.integer(2) ,
                                         MONTH %in% c(7:9) ~ as.integer(3),
                                         MONTH %in% c(10:12) ~ as.integer(4),
                                         TRUE ~ as.integer(0))) %>%
    dplyr::mutate(MARKET_CODE = "UN") %>%
    dplyr::mutate(KEPTMT = dplyr::case_when(!is.na(KEPTMT) ~ KEPTMT,
                                            TRUE ~ 0)) %>%
    dplyr::mutate(DISCMT = dplyr::case_when(!is.na(DISCMT) ~ DISCMT,
                                            TRUE ~ 0)) %>%
    dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
    dplyr::summarise(SPPLIVMT = (KEPTMT+DISCMT),.groups="drop") %>%
    dplyr::select(YEAR,QTR,NEGEAR,MARKET_CODE,SPPLIVMT) %>%
    dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
    dplyr::summarise(landings_land = sum(SPPLIVMT)*kgTolbs*1000,
                     landings_nn = dplyr::n(),
                     .groups="drop")
  # full join to get sample numbers available with landings
  sampleData <- dplyr::full_join(sampleLandings,lengthDataSum,
                                               by=c("YEAR","QTR","NEGEAR","MARKET_CODE"))

  sampleLengths_161722_GB <- lengthData %>% dplyr::filter(YEAR <=2019 & YEAR >1963)
  sampleData_161722_GB <- sampleData %>% dplyr::filter(YEAR <=2019 & YEAR >1963)

  usethis::use_data(sampleLengths_161722_GB,overwrite=overwrite)
  usethis::use_data(sampleData_161722_GB,overwrite=overwrite)

  ## length weight data same format as pulling from svdbs
  ll<- lengthWeightData %>%
    dplyr::select(YEAR,QTR,LENGTH,SAMPWEIGHT,SEX) %>%
    dplyr::mutate(INDWT = SAMPWEIGHT/1000) %>% # convert to kg
    dplyr::mutate(SEASON = dplyr::case_when(QTR == 1 ~ "SPRING",
                                            QTR == 2 ~ "SUMMER",
                                            QTR == 3 ~ "FALL",
                                            QTR == 4 ~ "WINTER",
                                            TRUE ~ "NA")) %>%
    dplyr::mutate(SEXX = dplyr::case_when(SEX == "Male" ~ 1,
                                          SEX == "Female" ~ 2,
                                          TRUE ~ 0)) %>%
    dplyr::mutate(CRUISE6 = paste0(YEAR,"00")) %>%
    dplyr::select(CRUISE6,SEASON,SEXX,LENGTH,INDWT) %>%
    dplyr::rename(SEX = SEXX) %>%
    tibble::as_tibble()

  lwd <- list(data=ll,sql=NA,colNames=NA)
  saveRDS(lwd,file=here::here("data-raw/data/",paste0("lengthWeight32.rds")))

}


