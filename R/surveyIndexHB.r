#' The survey biomass indices for the focal species within the mskeyrun project 
#' from the Bigelow era
#'
#' Survey data comes from Northeast Fisheries Science Center's Bottom Trawl
#' Survey. Indices are calculated using the \code{Survdat} R package.  This data 
#' set covers the Bigelow era of the bottom trawl survey from 2009 to 2019.
#' No calibration factors have been applied for the change from Albatross IV to
#' Bigelow.  Data from the Albatross era (1968 - 2008) are in a separate data set 
#' (\code{surveyIndexA4}).  This data only covers the focal species of the MS Keyrun
#' Project.  For an expanded data set see \code{surveyIndexHBAll}.
#'
#' @format A data frame
#' @family surveyIndex
#'
#' \describe{
#'\item{YEAR}{year survey conducted}
#'\item{SVSPP}{Survey Species Code see \code{focalSpecies}}
#'\item{SEASON}{Season the survey was conducted}
#'\item{variable}{stratified biomass or abundance (strat.biomass/strat.abund), 
#'     variance (biomass.var/abund.var), or standard error (biomass.SE/abund.SE)}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'
#' }
#'
#'
"surveyIndexHB"
