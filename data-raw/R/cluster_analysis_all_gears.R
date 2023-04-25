#' Perform cluster analysis
#' 
#' Uses all gear Types and species contributing to x% of landings for each gear type.
#' Reads in preprocessed data from get_landings_by_gear.R
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

library(magrittr)

cluster_analysis_all_gears <- function(){

  # read in landings data by gear/species aggregated over time
  allGearData <- readRDS(here::here("data-raw/data","gearLandingsBySpecies.rds"))
  # read in all gear codes
  gearCodes <- readRDS(here::here("data-raw/data","gearCodeTable.rds"))
  
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
  
  # now joint unique code names to main gear data table
  gearTable <- allGearData$data %>% dplyr::left_join(gearCodesUpdate,by="NEGEAR2") %>%
    dplyr::mutate(GEARID=paste0(NEGEAR2,"-",GEARName)) %>%
    dplyr::select(-NEGEAR2,-GEARName)
  

  # run clustering
  clusterObj <- cluster_gears(gearTable)
  
  return(clusterObj)
  
}


  
