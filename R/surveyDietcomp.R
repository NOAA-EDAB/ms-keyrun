#' The survey diet compositions for the focal species within the mskeyrun project 
#'
#' Survey diet data from Northeast Fisheries Science Center's Bottom Trawl Survey, 1973-2019.
#'
#' @format A data frame
#' 
#'  \describe{
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{year}{year diet data collected}
#'\item{Code}{SPECIES_ITIS code for predator species}
#'\item{Name}{Common name for predator species}
#'\item{season}{season diet data collected}
#'\item{prey}{Common name for prey item}
#'\item{variable}{mean prey item weight in year/species/season (meansw),
#'                variance of meansw (variance),
#'                cv of meansw (cv),
#'                confidence interval for mean prey weight in diet (ci),
#'                total prey weight in year/species/season (totwt),
#'                percent of prey item in diet in year/species/season (relmsw),
#'                confidence interval for percent prey in diet (relci),
#'                number of tows in year/species/season (num_tows),
#'                number of stomachs in year/species/season (nstom)}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable; grams for meansw, variance, ci, totwt,
#'             percent for relmsw, relci, numbers for num_tows, nstom, unitless cv}
#' }
#' 
"surveyDietcomp"

