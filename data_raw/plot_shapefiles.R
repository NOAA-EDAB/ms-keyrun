#' Plots the shape files in the gis directory
#'

# combines EPU and survey strata
EPU_strata <- sf::st_read(here::here("gis","EPU_strata.shp"),quiet=T)

ggplot2::ggplot(data=EPU_strata) +
  ggplot2::geom_sf(ggplot2::aes(fill=Strata))

# combines EPU and statistical area
EPU_stat <- sf::st_read(here::here("gis","GB_EPU_stat.shp"))

ggplot2::ggplot(data=EPU_stat) +
  ggplot2::geom_sf(ggplot2::aes(fill=Id))

# all survey strata
strata <- sf::st_read(here::here("gis","strata.shp"))

ggplot2::ggplot(data=strata) +
  ggplot2::geom_sf(ggplot2::aes(fill=STRATA))

# EPU 10  min square definitions
EPU10 <- sf::st_read(here::here("gis","EPU_extended.shp"))

ggplot2::ggplot(data=EPU10) +
  ggplot2::geom_sf(ggplot2::aes(fill=EPU))

# NEW GB strata  min square definitions
GB_strata <- sf::st_read(here::here("gis","GB_SOE_strata.shp"))

ggplot2::ggplot(data=GB_strata) +
  ggplot2::geom_sf(ggplot2::aes(fill=EPU))


