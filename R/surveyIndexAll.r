#' The survey biomass indices for all species within the mskeyrun project
#'
#' Survey data comes from Northeast Fisheries Science Center's Bottom Trawl
#' Survey. Indices are calculated using the \code{Survdat} R package.  This data 
#' set covers the entire bottom trawl survey from 1968 to 2019.
#' All appropriate calibration factors have been applied for changes in gear/vessels
#' over the years.  Data from the Bigelow era (>2008) that are not converted 
#' are in a separate data set (\code{surveyIndexHBAll}).  This data covers all 
#' species caught on the survey. For the data set containing only the focal 
#' species see \code{surveyIndex}.
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
"surveyIndexAll"
