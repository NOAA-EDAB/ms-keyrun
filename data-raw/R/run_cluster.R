#' Run cluster analysis and time series plots plots
#' 
#' gear type data are pulled from database and most abundant are plotted
#' 
#' 

source(here::here("R","cluster_analysis_filter_gears.R"))

clusterOut <- cluster_analysis_filter_gears(channel,filterByLandings=.99)

for (gear in clusterOut$topGears) {
  explore_landings_by_gear(channel,data=gearData$data,gearCode=gear,filterYrs=0.30,saveToFile=T)
}


  
