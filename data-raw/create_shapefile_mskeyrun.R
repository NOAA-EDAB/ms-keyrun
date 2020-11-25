#' create new shapefile which has statareas and GB intersections as seperate region
#' 

library(magrittr)

create_shapefile_mskeyrun <- function() {

  # read in Statareas and select GB stat areas
  GBStatAreas <- c(cfdbs::EPUs$data$GB,537)
  statAreas <- sf::st_read(dsn=here::here("gis","Statistical_Areas_2010.shp"),quiet=T) %>%
    dplyr::filter(Id %in% GBStatAreas)
  
  # read in GB EPU
  
  GB <- sf::st_read(dsn=here::here("gis","GB_SOE_strata.shp"),quiet=T) %>%
    dplyr::filter(EPU == "GB")
  
  GB <- sf::st_transform(GB,crs=sf::st_crs(statAreas))
  
  # intersection of GB with stat areas
  GBStat <- sf::st_intersection(statAreas,GB) %>%
    dplyr::mutate(Id=paste0(Id,"_in")) %>% 
    dplyr::select(Id,EPU)
  # non intersections of GB with stat areas
  GBNotStat <- sf::st_difference(statAreas,GB) %>%
    dplyr::mutate(Id=paste0(Id,"_out")) %>% 
    dplyr::select(Id,EPU)
  # combine shape files 
  GBShape <- rbind(GBStat,GBNotStat)
  
  # write to file
  sf::write_sf(GBShape,dsn=here::here("gis","GB_SOE_strata_stat.shp"))
  
  # plots combined shapefile
  p1 <- ggplot2::ggplot(data=GBShape) +
    ggplot2::geom_sf(mapping=ggplot2::aes(fill=Id))
  print(p1)
  
  # plots nicer, overlays statareas on GB
  p2 <- ggplot2::ggplot(data=statAreas) +
    ggplot2::geom_sf(mapping=ggplot2::aes(fill=Id),alpha =0.3,fill ="grey") +
    ggplot2::geom_sf(data=GB,mapping=ggplot2::aes(fill=GB),alpha =0.3,fill ="blue")
  
  print(p2)
}