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

calc_all_species_proportions <- function() {
  
  # REVENUEFILE is a df where each record contains landings for a subtrip
  REVENUEFILE <- readRDS(here::here("data-raw","Landings_VTR_Geret_Data.rds")) # This stores a variable called REVENUEFILE
  
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
  
  return(data)
    # dplyr::filter(NESPP3 %in% nespp3codes) %>% 
    # dplyr::left_join(.,speciesNames,by="NESPP3") %>%
    # dplyr::rename(COMMON_NAME=COMMON_NAME.y)
  
  #calculate landings of all other fish combined
  otherFish <- REVENUEFILE %>%
    dplyr::select(Year,Area,NESPP3,InsideLANDED) %>% 
    dplyr::filter(!NESPP3 %in% nespp3codes) %>% 
    dplyr::filter(!Area == "Other") %>%
    dplyr::mutate(AREA=stringr::str_split_fixed(Area,"_",2)[,1]) %>%
    dplyr::mutate(InOut=stringr::str_split_fixed(Area,"_",2)[,2]) %>%
    dplyr::mutate(YEAR = as.numeric(Year)) %>% 
    dplyr::mutate(AREA = as.numeric(AREA)) %>% 
    dplyr::group_by(YEAR,AREA,InOut) %>%
    dplyr::summarise(TOTALLANDINGS=sum(InsideLANDED)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(COMMON_NAME="other Species",NESPP3="999")
  
  data <- rbind(data,otherFish)
  
  # otherSpecies <- REVENUEFILE %>%
  #   dplyr::filter(!Area == "Other") %>%  
  #   dplyr::filter(!NESPP3 %in% nespp3codes) %>% 
  #   dplyr::select(NESPP3,InsideLANDED) %>% 
  #   dplyr::group_by(NESPP3) %>%
  #   dplyr::summarise(TOTALLANDINGS=sum(InsideLANDED)) %>%
  #   dplyr::mutate(NESPP3=sprintf("%03d",NESPP3))
  # 
  # allOtherSpecies <- dbutils::create_species_lookup(channel,species = unique(otherFish$NESPP3)[-1],speciesType = "nespp3" )
  # 
  # a <- dplyr::left_join(allOtherSpecies$data,otherSpecies,by="NESPP3") %>%
  #   dplyr::arrange(desc(TOTALLANDINGS))
  # 
  saveRDS(data,file = here::here("data-raw","Landings_VTR_Geret_Data_summarized.rds"))
  
}
