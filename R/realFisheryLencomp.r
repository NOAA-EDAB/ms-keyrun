#' Fishery length composition for the mskeyrun project based on catch data
#'
#' Data pulled from the NEFSC commercial fishery database. Methods to obtain 
#' catch at length compositions are described in \code{\link{mscatch}} package
#' 
#' @format A data frame
#'
#' \describe{
#'\item{ModSim}{Atlantis model name and simulation id, or Actual data}
#'\item{year}{year of catch}
#'\item{Code}{Species itis code}
#'\item{Name}{Common name for species}
#'\item{fishery}{fishery name}
#'\item{lenbin}{fishery recorded 1 cm length bin}
#'\item{variable}{metric recorded}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'
#' }
#'
#'
"realFisheryLencomp"
