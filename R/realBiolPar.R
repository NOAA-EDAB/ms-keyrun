#' The focal species biological parameters in mskeyrun project
#'
#' Length (cm) weight (g) parameters based on bottom trawl survey data. See details below
#' 
#' Model: W = aL^b e^Z
#' where Z ~ IIDN(0,sigma^2)
#' 
#' E(W) = aL^b e^((sigma^2)/2)
#' 
#' @format A data frame
#'
#' \describe{
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{Code}{Itis code for species}
#'\item{Name}{Common name for species}
#'\item{WLa}{Weight-Length equation parameter a, W = aL^b}
#'\item{WLb}{Weight-Length equation parameter b, W = aL^b}
#'\item{sigma}{Estimated standard deviation of the Weight-Length equation parameter W = aL^b}
#' }
#'
#'
"realBiolPar"