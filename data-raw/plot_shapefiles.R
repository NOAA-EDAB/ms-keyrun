#' Plots the shape files in the gis directory
#'

# combines EPU and survey strata
EPU_strata <- sf::st_read(here::here("gis","EPU_strata.shp"),quiet=T)

ggplot2::ggplot(data=EPU_strata) +
  ggplot2::geom_sf(ggplot2::aes(fill=Strata))

# combines EPU and statistical area
stat_Areas <- sf::st_read(here::here("gis","Statistical_Areas_2010.shp"))

ggplot2::ggplot(data=stat_Areas) +
  ggplot2::geom_sf(ggplot2::aes(fill=Id))

# all survey strata
strata <- sf::st_read(here::here("gis","strata.shp"))

ggplot2::ggplot(data=strata) +
  ggplot2::geom_sf(ggplot2::aes(fill=STRATA))

# EPU 10  min square definitions
EPU10 <- sf::st_read(here::here("gis","EPU_extended.shp"))

ggplot2::ggplot(data=EPU10) +
  ggplot2::geom_sf(ggplot2::aes(fill=EPU))

# NEW GB EPU def based on survey strata
GB_strata <- sf::st_read(here::here("gis","GB_SOE_strata.shp"))

ggplot2::ggplot(data=GB_strata) +
  ggplot2::geom_sf(ggplot2::aes(fill=EPU))

# GB strata min square definitions
GB_EPU_stat <- sf::st_read(here::here("gis","GB_SOE_strata_stat.shp"))

ggplot2::ggplot(data=GB_EPU_stat) +
  ggplot2::geom_sf(ggplot2::aes(fill=Id))
