#' The survey biomass indices for all species within the mskeyrun project 
#' from the Bigelow era
#'
#' Survey data comes from Northeast Fisheries Science Center's Bottom Trawl
#' Survey. Indices are calculated using the \code{Survdat} R package.  This data 
#' set covers the Bigelow era of the bottom trawl survey from 2009 to 2019.
#' No calibration factors have been applied for the change from Albatross IV to
#' Bigelow.  Data from the Albatross era (1968 - 2008) are in a separate data set 
#' (\code{surveyIndexA4All}).  This data covers all species caught on the survey.
#' For the data set containing only the focal species see \code{surveyIndexHB}.
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
"surveyIndexHBAll"
