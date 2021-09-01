#' Proportion of species straddling Georges Bank EPU
#'
#' Georges Bank EPU is defined as an area within a subset of predefined Statistical fishing areas.
#' Landings from a statistical area were [attributed to Georges Bank](https://noaa-edab.github.io/ms-keyrun/allocateLandingsEPU.html) by determining how variable landings were over time and if constant assign a 
#' fixed proportion of the landings to the Georges Bank region
#'
#' @format A data frame with 4 columns
#'
#' \describe{
#'   \item{AREA}{Statistical Area code}
#'   \item{NESPP3}{species code from commerical fishing database}
#'   \item{InOut}{Designation of insie or outside Georges Bank}
#'   \item{MeanProp}{Mean proportion of landings within AREA for each species}
#'
#' }
#'
#'
"AllSpeciesProportions"
