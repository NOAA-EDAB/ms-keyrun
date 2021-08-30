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

calc_all_species_proportions <- function() {
  
  # REVENUEFILE is a df where each record contains landings for a subtrip
  REVENUEFILE <- readRDS(here::here("data-raw/data","Landings_VTR_Geret_Data.rds")) # This stores a variable called REVENUEFILE
  
  # split the are column to define inside and out, sum landings by year, spp, area 
  data <- REVENUEFILE %>%
    dplyr::select(Year,Area,NESPP3,InsideLANDED) %>% 
    dplyr::filter(!Area == "Other") %>%
    dplyr::mutate(AREA=stringr::str_split_fixed(Area,"_",2)[,1]) %>%
    dplyr::mutate(InOut=stringr::str_split_fixed(Area,"_",2)[,2]) %>%
    dplyr::mutate(YEAR = as.numeric(Year)) %>% 
    dplyr::mutate(AREA = as.numeric(AREA)) %>% 
    dplyr::group_by(YEAR,AREA,NESPP3,InOut) %>%
    dplyr::summarise(TOTALLANDINGS=sum(InsideLANDED),.groups="drop")
    # calculate proportion of landings inside GB section of stat area
  props <- data %>% 
      dplyr::group_by(YEAR,AREA,NESPP3) %>% 
      dplyr::summarise(PROP_GB = calc_prop(TOTALLANDINGS,InOut),.groups = "drop")
 
  saveRDS(props,file = here::here("data-raw/data","Landings_VTR_proportions_all_species.rds"))
  
}
