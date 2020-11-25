#'Reads in output from VTR/raster/DMIS (Dennis Corvi offshoreWind package)
#'
#'Calculates the proportion of landings over time for species in Statareas/EPU intersection
#'
#'
#'@section Process to obtain data
#'
#'offshoreWind package must be installed and run on the server (mars)
#'1. Copy shapefiles "GB_SOE_strata_stat" onto the server
#'2. install  remotes::install_github("dcorvi/offshoreWind",auth_token="createOne") - it's a private repo
#'3. offshoreWind::getCallAreaEffort(shapefile_f = "/net/home5/abeet/shapefiles/GB_stat_EPU",
#'                                  output_f = "/net/home5/abeet/offshoreWind/out",
#'                                  start = 1994, end = 2018)
#'4. offshoreWind::get_revenuefile(project_name="Landings_VTR_Geret_Data",
#'                                output_folder_name = "/net/home5/abeet/offshoreWind/out",
#'                                control_data=FALSE,
#'                                save_files = "r",
#'                                area_data_path="/net/home5/abeet/offshoreWind/out")
#' see get_landings_vtr_geret.r
library(magrittr)

calc_prop <- function(landings,inout) {
  prop <- sum(landings*(inout=="in"))/sum(landings)
  return(prop)
}

plot_landings_proportions <- function(species="cols") {
   # REVENUEFILE is a df where each record contains landings for a subtrip
  REVENUEFILE <- readRDS(here::here("data-raw","Landings_VTR_Geret_Data_summarized.rds"))
  
  # calculate proportion of landings inside GB section of stat area
  data <- REVENUEFILE %>% 
    dplyr::group_by(YEAR,AREA,NESPP3,COMMON_NAME) %>% 
    dplyr::summarise(PROP_GB = calc_prop(TOTALLANDINGS,InOut))

  if(species=="cols") {
    # plot grid area by species
    p <- ggplot2::ggplot(data=data) +
      ggplot2::geom_line(mapping = ggplot2::aes(x=YEAR,y=PROP_GB)) +
      ggplot2::facet_grid(cols = ggplot2::vars(COMMON_NAME), rows= ggplot2::vars(AREA)) +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = 5)) +
      ggplot2::theme(axis.text.x=ggplot2::element_text(size=8,angle=90)) +
      ggplot2::theme(axis.text.y=ggplot2::element_text(size=7)) +
      ggplot2::ylab("Proportion of landings") + 
      ggplot2::xlab("")
  
    print(p)
  } else if (species =="rows") {
  
    # plot grid area by species
    p <- ggplot2::ggplot(data=data) +
      ggplot2::geom_line(mapping = ggplot2::aes(x=YEAR,y=PROP_GB)) +
      ggplot2::facet_grid(cols = ggplot2::vars(AREA), rows= ggplot2::vars(COMMON_NAME)) +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = 7)) +
      ggplot2::theme(strip.text.y = ggplot2::element_text(size = 5)) +
      ggplot2::theme(axis.text.x=ggplot2::element_text(size=8,angle=90)) +
      ggplot2::theme(axis.text.y=ggplot2::element_text(size=6)) +
      ggplot2::ylab("Proportion of landings") + 
      ggplot2::xlab("")
    
    print(p)
  } else {
    stop(message("species must be \"rows\" or \"cols\""))
  }

}
