#' reads in length data for winter skate
#'
#' Data pulled from svdbs. Since single stock. use all lengths and apply
#' to GB landings
#'
#'
#'

#data <- survdat::get_length_weight_data()
library(magrittr)


format_skate_data <- function(overwrite=F) {

  kgTolbs <- 2.2046226218 #(1kg = 2.2046226218 lbs)
  #length data. use entire shelf since one stock
  lengthData <- readRDS(here::here("Data-raw/data","lengthWeight23.rds"))$data

  lengthData <- lengthData %>%
    dplyr::mutate(tripid = paste0(paste0(CRUISE6,STRATUM,STATION,TOW))) %>%
    dplyr::mutate(YEAR = substr(CRUISE6,1,4)) %>%
    dplyr::select(YEAR,SEASON,LENGTH,tripid) %>%
    dplyr::group_by(YEAR,SEASON,LENGTH,tripid) %>%
    dplyr::summarise(NUMLEN = dplyr::n(),.groups="drop") %>%
    dplyr::mutate(NEGEAR = "050",MARKET_CODE = "UN") %>%
    dplyr::mutate(QTR = dplyr::case_when(SEASON == "SPRING" ~ 1,
                                         SEASON == "SUMMER" ~ 2 ,
                                         SEASON == "FALL" ~ 3,
                                         SEASON == "WINTER" ~ 4,
                                         TRUE ~ 0)) %>%
    dplyr::mutate(YEAR = as.integer(YEAR),
                  QTR = as.integer(QTR)) %>%
    dplyr::select(YEAR,QTR,NEGEAR,MARKET_CODE,LENGTH,NUMLEN,tripid)


  lengthDataSum <- lengthData %>%
    dplyr::group_by(YEAR, QTR, NEGEAR, MARKET_CODE) %>%
    dplyr::summarize(len_totalNumLen=sum(as.numeric(NUMLEN)),len_numLengthSamples=length(unique(tripid)),.groups="drop")

  # landings data from comlandr for GB region only
  sampleLandings <- readRDS(here::here("data-raw/data/comland.rds"))$comland %>%
    dplyr::filter(NESPP3 == 367) %>%
    dplyr::filter(AREA %in% c(521,522,523,524,525,526,538,551,552,561,562,537)) %>%
    dplyr::mutate(YEAR = as.integer(YEAR)) %>%
    dplyr::mutate(QTR = dplyr::case_when(MONTH %in% c(1:3) ~ as.integer(1),
                                         MONTH %in% c(4:6) ~ as.integer(2) ,
                                         MONTH %in% c(7:9) ~ as.integer(3),
                                         MONTH %in% c(10:12) ~ as.integer(4),
                                         TRUE ~ as.integer(0))) %>%
    dplyr::mutate(MARKET_CODE = dplyr::case_when(MKTCAT %in% c(0,1,8,9) ~ "UN",
                                                 MKTCAT %in% c(2,4) ~ "SQ" ,
                                                 MKTCAT %in% c(3,5) ~ "LG",
                                                 TRUE ~ "UN")) %>%
    dplyr::mutate(NEGEAR = as.character(NEGEAR)) %>%
    dplyr::mutate(NEGEAR = dplyr::case_when(nchar(NEGEAR) == 2 ~ paste0(0,NEGEAR),
                                            nchar(NEGEAR) == 3 ~ NEGEAR,
                                            TRUE ~ "999")) %>%
    dplyr::select(YEAR,QTR,NEGEAR,MARKET_CODE,SPPLIVMT) %>%
    dplyr::group_by(YEAR,QTR,NEGEAR,MARKET_CODE) %>%
    dplyr::summarise(landings_land = sum(SPPLIVMT)*kgTolbs*1000, # convert to lbs
                     landings_nn = dplyr::n(),
                     .groups="drop")
  # full join to get sample numbers available with landings
  sampleData <- dplyr::full_join(sampleLandings,lengthDataSum,
                                               by=c("YEAR","QTR","NEGEAR","MARKET_CODE"))

  sampleLengths_564145_GB <- lengthData %>% dplyr::filter(YEAR <=2019)
  sampleData_564145_GB <- sampleData %>% dplyr::filter(YEAR <=2019)

  usethis::use_data(sampleLengths_564145_GB,overwrite=overwrite)
  usethis::use_data(sampleData_564145_GB,overwrite=overwrite)

}


