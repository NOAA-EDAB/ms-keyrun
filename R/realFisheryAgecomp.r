#' Fishery Age composition for the mskeyrun project based on catch data
#'
#' Data pulled from the NEFSC commercial fishery database. Methods to obtain 
#' catch at age compositions are described in \code{\link{mscatch}} package
#' 
#' @format A data frame
#'
#' \describe{
#'\item{ModSim}{Atlantis model name and simulation id, or Actual data}
#'\item{year}{year of catch}
#'\item{Code}{Species itis code}
#'\item{Name}{Common name for species}
#'\item{fishery}{fishery name}
#'\item{age}{age of fish in fishery, yrs}
#'\item{variable}{metric recorded, Number at age}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'
#' }
#'
#'
"realFisheryAgecomp"
