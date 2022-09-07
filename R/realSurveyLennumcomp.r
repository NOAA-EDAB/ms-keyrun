#' Survey length composition for the mskeyrun project based on bottom trawl survey data
#'
#' Data pulled from the NEFSC bottom trawl survey database. Data pulled and processed using 
#'  \code{\link{survdat}} package.
#' 
#' @format A data frame
#'
#' \describe{
#'\item{ModSim}{Atlantis model name and simulation id, or Actual data}
#'\item{year}{year in which survey was undertaken}
#'\item{season}{season in which survey was undertaken}
#'\item{Code}{Species itis code}
#'\item{Name}{Common name for species}
#'\item{fishery}{fishery name}
#'\item{lenbin}{recorded 1 cm length bin}
#'\item{variable}{metric recorded, biomass & numbers}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'
#' }
#'
#'
"realSurveyLennumcomp"
