#' The Atlantis-simulated survey characteristics for the mskeyrun project
#'
#' Simulated survey information: lookup table for species and survey names with
#' survey cv, timing, and general spatial coverage
#'
#' @format A data frame
#'
#' \describe{
#'\item{Code}{Atlantis model three letter code for functional group}
#'\item{Name}{Atlantis model common name for functional group}
#'\item{survey}{simulated survey name}
#'\item{cv}{survey coefficient of variation (cv) of biomass}
#'\item{survMonth}{survey month of year converted from Atlantis output timestep}
#'\item{survArea}{survey coverage of Atlantis model polygons; "All" or "Subset"}
#'
#' }
#'
#'
"simSurveyInfo"
