#' plot stock areas
#'
#'
#'
#'

plot_stock_areas <- function(coast,statareas,GB,stockArea,keyrunAreas,addAreas=NULL) {


  #obj <- mscatch::get_species_object(species_itis = itis,stock = stock)
  #msk <- mscatch::get_species_object_mskeyrun(species_itis = itis)
  
  keyrunAreasSp <- statareas %>% 
    dplyr::filter(Id %in% keyrunAreas)
  
  areas <- statareas %>% 
    dplyr::filter(Id %in% stockArea)
  
  bb <- sf::st_bbox(areas)
  xmin <- min(bb$xmin,-76)
  xmax <- max(bb$xmax,-65)
  ymin <- min(bb$ymin,35)
  ymax <- max(bb$ymax,45)
  
  p <- ggplot2::ggplot(data=coast) +
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data= GB,col="grey",alpha = 0.8) +
    ggplot2::geom_sf(data= areas,col="black",fill="lightgrey",alpha = 0.5) +
    ggplot2::geom_sf(data= keyrunAreasSp,col="coral",fill="coral",alpha = 0.25) 
  
  if (!is.null(addAreas)) {
    p <-  p +  
      ggplot2::geom_sf(data = addAreas,col="black",fill="lightgrey",alpha = 0.2)
    areas = rbind(areas,addAreas)
  }
  
  p <- p +
    ggplot2::coord_sf(xlim = c(xmin,xmax), ylim = c(ymin,ymax)) +
    ggplot2::geom_text(data=areas,ggplot2::aes(x=X,y=Y,label=Id),size=2) +
    ggplot2::xlab("") +
    ggplot2::ylab("")

    

  
  return(p)
}