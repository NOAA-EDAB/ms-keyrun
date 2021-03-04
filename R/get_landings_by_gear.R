#' Read in landings data by gear and save as RDS
#' 
#' cfdbs is accessed and landings pulled by gear type for GB over time
#' Data then aggregated over trips for each year and saved as RDS
#' Data also further aggregated over time and saved as RDS
#' 
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#' @param percentLanded Numeric scalar. Proportion [0,1] of landings to filter by
#' @param area Numeric vector. Statistical areas defining Area from which landings data should be pulled.
#' @param outDir Character string. Full path to directory where output should be saved. Default=here::here("data")
#' 
#' @return Two RDS files saved

library(magrittr)

get_landings_by_gear <- function(channel,percentLanded=0.90,area=c(cfdbs::EPUs$data[["GB"]],537),outDir=here::here("data")) {

  mGearData <- NULL
  mTimeGearData <- NULL
  gearCodes <- cfdbs::fleets$codes
  message(paste0("Pulling data for following gear Codes: ",paste0(gearCodes,collapse=",")))
  for (gear in gearCodes){
    message(paste0("gear Code = ",gear))
    gearData <- cfdbs::get_landings(channel,area=area,gear=gear, year="all",tonnage = "all",species = "all")
    
    # sum over trips by year group by gear and species
    aggTimeGear <- gearData$data %>% 
      dplyr::group_by(YEAR,NESPP3) %>% 
      dplyr::summarise(totsplandlb=sum(as.numeric(SPPLNDLB)),n=dplyr::n()) %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(NEGEAR2=gear)
    
    # sum over time group by gear and species
    aggGear <- gearData$data %>% 
      dplyr::group_by(NESPP3) %>% 
      dplyr::summarise(totsplandlb=sum(as.numeric(SPPLNDLB)),n=dplyr::n()) %>% 
      dplyr::ungroup()
    
    # select species that account for x% of landings
    orderedSpecies <- aggGear %>% 
      dplyr::group_by(NESPP3) %>% 
      dplyr::summarise(sumOverYears=sum(as.numeric(totsplandlb))) %>% # total by species agg over time
      dplyr::arrange(desc(sumOverYears)) %>% 
      dplyr::mutate(csum=cumsum(sumOverYears)) %>%
      dplyr::mutate(prop = csum/sum(sumOverYears))
    
    orderSpecies <- orderedSpecies %>% 
      dplyr::filter(prop <= percentLanded)
    
    if (dim(orderSpecies)[1]==0){
      # dominated by one species > percentLanded
      orderSpecies <- orderedSpecies[1,]
    }
  
    # select these species from data pull to account for gear species interaction
    useData <- aggGear %>% 
      dplyr::filter(NESPP3 %in% orderSpecies$NESPP3) %>% 
      dplyr::select(-n) %>% 
      dplyr::mutate(NEGEAR2=gear)
    
    mGearData <- rbind(mGearData,useData)
    mTimeGearData <- rbind(mTimeGearData,aggTimeGear)
    
  }
  
  allGearData <- list(data=mGearData,Description=paste0("Species ordered by landings for each gear. Species comprising the top ",100*percentLanded,"% of the landings for each gear are retained")) 
  allTimeGearData <- list(data=mTimeGearData,Description=paste0("Time Series of species landings for each gear. All data")) 
  saveRDS(allGearData,file=paste0(outDir,"/gearLandingsBySpecies.rds"))
  saveRDS(allTimeGearData,file=paste0(outDir,"/timeSeriesSpeciesByGear.rds"))

}
