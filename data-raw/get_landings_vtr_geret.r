#' Run Corvi package. use VTR DMIS data
#'
#' THIS NEED TO BE RUN ON THE SERVER (mars.nefsc.noaa.gov)
#'

#'1. Copy shape file "GB_SOE_strata_stat.shp" onto the server eg. /net/home5/abeet/shapefiles/GB_SOE_strata_stat.shp
#'2. install  remotes::install_github("dcorvi/offshoreWind",auth_token="createOne") - it's a private repo



offshoreWind::getCallAreaEffort(shapefile_f = "/net/home5/abeet/shapefiles/GB_Stat_EPU",
                                  output_f = "/net/home5/abeet/offshoreWind/out",
                                  start = 1994, end = 2018)

offshoreWind::get_revenuefile(project_name="Landings_VTR_Geret_Data",
                                output_folder_name = "/net/home5/abeet/offshoreWind/out",
                                control_data=FALSE,
                                save_files = "r",
                                area_data_path="/net/home5/abeet/offshoreWind/out")

