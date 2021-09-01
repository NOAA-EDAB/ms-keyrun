#' Fleet designations based on NEGEAR2 codes
#'
#' NEGEAR2 codes are assigned to fleets based on which species are landed. 
#' The methodology and the decisions made to create this data set can be found below
#'
#' @format A data frame with 3 columns
#'
#' \describe{
#'   \item{fleetName}{Name of fleet}
#'   \item{NEGEAR2}{Gear codes associated with each fleet}
#'   \item{hydraFleets}{Fleet aggregation to satify Hydra multispecies model}
#'
#' }
#'
#' @section Methodology:
#'
#' This data set was created based on clustering [methods](https://noaa-edab.github.io/ms-keyrun/GBFleetDefinitions.html).
#'
#'
"fleets"
