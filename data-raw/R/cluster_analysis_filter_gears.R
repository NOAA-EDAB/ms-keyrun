#' Perform cluster analysis
#' 
#' Uses a filtered set of gear Types. 
#' gear types are chosen based on how many lbs in total is landed aggregated over time. 
#' landings are ordered by gear type an the gears contributing to the top x% of landings are retained
#' 
#' @param filterByLandings Numeric scalar. Proportion of landing to be captured by gears. (Default=.99)
#' 
#' @return object of class \code{agnes} representing the clustering. Use \code{plot} top display dendrogram
#' 
#' @example 
#' \dontrun{
#' clusterObj <- cluster_analysis_filter_gears()
#' # dendrogram plot
#' plot(clusterObj$top,ask=T,which.plots=2,main="Complete gear list using 99% landing from each gear",xlab="")
#' }
#' 
#' 

library(magrittr)
source(here::here("R","cluster_gears.r"))

cluster_analysis_filter_gears <- function(filterByLandings=.99){

  # read in landings data by gear/species aggregated over time
  #allGearData <- readRDS(here::here("data","gearLandingsBySpecies.rds"))
  allTimeGearData <- readRDS(here::here("data","timeSeriesSpeciesByGear.rds"))
  # read in all gear codes
  gearCodes <- readRDS(here::here("data","gearCodeTable.rds"))
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
  
  # aggregate data over time
  allTimeGearDataAgg <- allTimeGearData$data %>%
    dplyr::select(-n) %>% 
    dplyr::group_by(NESPP3,NEGEAR2) %>%
    dplyr::summarise(totsplandlb = sum(totsplandlb))
    

  # now joint unique code names to main gear data table
  gearTable <- allTimeGearDataAgg %>% dplyr::left_join(gearCodesUpdate,by="NEGEAR2") %>%
    dplyr::mutate(GEARID=paste0(NEGEAR2,"-",GEARName)) %>%
    dplyr::select(-GEARName)
 
  # Prep data for analysis --------------------------------------------------
  # aggregate landings over time for each gear type
  gd <- allTimeGearData$data %>%
    dplyr::select(-NESPP3) %>%
    dplyr::group_by(NEGEAR2) %>%
    dplyr::summarise(totLandingslb=sum(totsplandlb)) %>%
    dplyr::arrange(desc(totLandingslb)) %>%
    dplyr::mutate(cumusum = cumsum(totLandingslb)) %>%
    dplyr::mutate(proportion = cumusum/sum(totLandingslb))
  # pick top species that meet minimum landings percentage criterion 
  ind <- c(T,gd$proportion <= filterByLandings)
  ind <- head(ind,-1)
  topData <- gd[ind,]

  topTable <- gearTable %>% 
    dplyr::filter(NEGEAR2 %in% topData$NEGEAR2)
  
  topGears <- unique(topTable$NEGEAR2)
  
  topTable <- topTable %>%
    dplyr::select(-NEGEAR2)

  ## cluster analysis on top x%
  top <- cluster_gears(topTable)
  
  ## cluster analysis on bottom 100-x%
  bottomData <- gd[!ind,]
  bottomTable <- gearTable %>% 
    dplyr::filter(NEGEAR2 %in% bottomData$NEGEAR2)
  
  bottomGears <- unique(bottomTable$NEGEAR2)
  
  bottomTable <- bottomTable %>%
    dplyr::select(-NEGEAR2)
  
  
  bottom <- cluster_gears(bottomTable)
  
  return(list(top=top,topGears=topGears,bottom=bottom,bottomGears=bottomGears))
  
}


  
