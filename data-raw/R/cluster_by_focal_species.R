#' Perform cluster analysis
#' 
#' Uses all gear Types that catch any amount of the species of interest
#' Reads in preprocessed data from get_landings_by_gear.r
#' 
#' @return object of class \code{agnes} representing the clustering. Use \code{plot} top display dendrogram
#' 
#' @example 
#' \dontrun{
#' clusterObj <- cluster_analysis_all_gears()
#' # dendrogram plot
#' plot(clusterObj,ask=T,which.plots=2,main="Complete gear list using 90% landing from each gear",xlab="")
#' }
#' 
#' 

source(here::here("R","cluster_gears.r"))
library(magrittr)
focalSpecies <- readRDS(here::here("data","focalspecies.rds"))

cluster_by_focal_species <- function(focalSpecies){

  # read in landings data by gear/species aggregated over time
  allGearData <- readRDS(here::here("data","timeSeriesSpeciesByGear.rds"))
  # read in all gear codes
  gearCodes <- readRDS(here::here("data","gearCodeTable.rds"))
  
  ## PROCESS GEAR CODES -----------------------------------
  # split long names by comma and select first name
  gearCodes <- gearCodes$data %>% 
    dplyr::select(NEGEAR2,GEARNM) %>%
    dplyr::mutate(GEARNM2=unlist(lapply(stringr::str_split(gearCodes$data$GEARNM,","),`[[`,1))) %>% 
    dplyr::select(-GEARNM) %>% 
    dplyr::distinct(NEGEAR2,GEARNM2)
  
  # find unique gear codes, make single string if has multiple entries and create new table
  uniqueCodes <- unique(gearCodes$NEGEAR2)
  gearCodesUpdate <- data.frame(matrix(nrow = length(uniqueCodes),ncol=2))
  names(gearCodesUpdate) <- c("NEGEAR2","GEARName")
  for (icode in 1:length(uniqueCodes)) {
    ind <- gearCodes$NEGEAR2 == uniqueCodes[icode]
    if(sum(ind) > 1) {
      newCode <- paste0(gearCodes$GEARNM2[ind],"/",collapse=" ")
      gearCodesUpdate$NEGEAR2[icode] <- as.numeric(uniqueCodes[icode])
      gearCodesUpdate$GEARName[icode] <-substr(newCode,1,10) # shortens name to 10 characters for ease in plotting
    } else {
      gearCodesUpdate$NEGEAR2[icode] <- as.numeric(uniqueCodes[icode])
      gearCodesUpdate$GEARName[icode] <- gearCodes$GEARNM2[ind]
    }
  }
  
  ## Process time series data to select gears that catch focal species
  gearsToUse <- allGearData$data %>% 
    dplyr::filter(NESPP3 %in% focalSpecies$NESPP3) %>%
    dplyr::distinct(NEGEAR2) %>% 
    dplyr::pull()
  
  ## filter data by these gear types and join to gearcode table for short gear names
  gearTable <- allGearData$data %>% 
    dplyr::select(-n) %>%
    dplyr::filter(NEGEAR2 %in% gearsToUse) %>%
    dplyr::group_by(NESPP3,NEGEAR2) %>%
    dplyr::summarise(totsplandlb = sum(totsplandlb)) %>%
    dplyr::left_join(gearCodesUpdate,by="NEGEAR2") %>%
    dplyr::mutate(GEARID=paste0(NEGEAR2,"-",GEARName)) %>%
    dplyr::select(-NEGEAR2,-GEARName)

  
  # pick one species
  #speciesTable <- gearTable %>%
  #  dplyr::filter(NESPP3 == "081")
  #return(speciesTable)

  # run clustering
  clusterObj <- cluster_gears(gearTable)
  
  return(clusterObj)
  
}


  
