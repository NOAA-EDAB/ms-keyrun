#' The fishery catch indices by gear for the mskeyrun project
#' 
#' CURRENTLY A TEMPORARY FIX FOR NAs IN CATCH INDEX FOR SIMULATIONS
#' DO NOT PUT THIS IN MSKEYRUN MAIN BRANCH: FIX NA IN SOURCE DATA INSTEAD
#' DO NOT USE THIS AS ACTUAL GEORGES BANK DATA
#'
#' @format A data frame
#'
#'#'
#'@return A tibble (Also written to \code{data} folder)
#'\item{variable}{commercial landings or commercial discards}
#'\item{modelName}{species modelName as specified in\code{mskeyrun::focalSpecies} }
#'\item{YEAR}{year of catch}
#'\item{hydraFleets}{3 fleets for the hydra model as specified in \code{mskeyrun::fleets}}
#'\item{valueNAfill}{estimated landings or discards with NAs filled according to proportions by non-na gear}
#'\item{units}{metric tons}
#'
#'
#'
"catchIndexGearNAfill"
