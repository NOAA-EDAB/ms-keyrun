#' Explore landings 
#' 
#' Explores time series of species caught by gear type
#' 
#' @param channel 
#' @param data Data frame. landings data by YEAR, NESPP3, NEGEAR2 (long format)
#' @param gearCode Numeric. NEGEAR2 code
#' @param filterYrs Numeric. A proportion reflecting the proportion of years containing data for a particucar species (eg.)
#' A value of .15 retains only the species in which there were landings in more that 15% of the years sampled
#' @param saveToFile Boolean. Save to doc/figures folder
#' 
#' @return 
#' 
#' @example 
#' \dontrun{
#' }
#' 
#' 

library(magrittr)
library(ggplot2)

gearData <- readRDS(file=paste0(here::here("data","timeSeriesSpeciesByGear.rds")))

explore_landings_by_gear <- function(channel,data=gearData$data,gearCode=5,filterYrs=0.30,saveToFile=F){
  
  # pull gear Specific data
  gd <- data %>%
    dplyr::filter(NEGEAR2==gearCode) %>%
    dplyr::mutate(NESPP3 = as.numeric(NESPP3),YEAR=as.numeric(YEAR))
  
  # yr range
  yrRange <- range(as.numeric(gd$YEAR))

  # find species that are not abundant
  filteredSpecies <- gd %>% 
    dplyr::group_by(NESPP3) %>%
    dplyr::summarise(n2=dplyr::n()) %>% 
    dplyr::filter(n2 > filterYrs*diff(yrRange))
  
  speciesLeftOut <- sort(setdiff(unique(gd$NESPP3),filteredSpecies$NESPP3))

  # filter out species from data set
  gd2 <- gd %>% 
    dplyr::filter(NESPP3 %in% filteredSpecies$NESPP3)
  
  # calculate % of landings lost due to filtering out non abundant species
  landingsLeftOut <- gd %>% dplyr::filter(NESPP3 %in% speciesLeftOut) %>%
    dplyr::select(totsplandlb) %>% 
    sum()
  totalLandings <- gd %>% 
    dplyr::select(totsplandlb) %>% 
    sum()
  
  message(paste0("Percentage of landings due to omitted species = ",100*landingsLeftOut/totalLandings))

  # sort in order of abundance
  # aggregate landings over time for each gear type
  gd3 <- data %>%
    dplyr::filter(NEGEAR2 == gearCode) %>%
    dplyr::group_by(NESPP3) %>%
    dplyr::summarise(totLandingslb=sum(totsplandlb)) %>%
    dplyr::arrange(desc(totLandingslb)) %>%
    dplyr::mutate(cumusum = cumsum(totLandingslb)) %>%
    dplyr::mutate(proportion = cumusum/sum(totLandingslb))
  # pick top species that meet minimum landings percentage criterion 
  ind <- c(T,gd3$proportion <= .95)
  ind <- head(ind,-1)
  topSpecies <- list(NESPP3=NULL)
  topSpecies$NESPP3 <- gd3[ind,]$NESPP3
  topSpecies <- as.data.frame(topSpecies)
  
  # pull species names 
  speciesNames <- dbutils::create_species_lookup(channel,species=topSpecies$NESPP3,speciesType = "NESPP3")
  speciesNames <- speciesNames$data %>%
    dplyr::select(NESPP3,COMMON_NAME) %>%
    dplyr::distinct() 
  
  # join names with top species. make factor for plotting
  ts <- topSpecies %>% 
    dplyr::left_join(.,speciesNames,by="NESPP3") %>%
    dplyr::mutate(NESPP3 = as.numeric(NESPP3))
  ts$COMMON_NAME <- factor(ts$COMMON_NAME,levels=ts$COMMON_NAME)

  # filter topSpecies and join to data
  gd2 <- gd2 %>% 
    dplyr::filter(NESPP3 %in% as.numeric(ts$NESPP3)) %>% 
    dplyr::left_join(.,ts,by="NESPP3")

 #plot time series of top species   
  p <- ggplot(data=gd2) +
    geom_line(mapping=aes(x=YEAR,y=totsplandlb/1e6)) +
    facet_wrap(vars(COMMON_NAME),scales="fixed") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(strip.text.x = element_text(size = 5)) +
    ggplot2::ylab("lbs (millions)")
    

  if(saveToFile){
    ggplot2::ggsave(filename = here::here("docs","figures",paste0("gear",sprintf("%02d",gearCode),".png")),
                    width=8,height=8, units="in")
  } else {
    plot(p)
  }
  
  
}
