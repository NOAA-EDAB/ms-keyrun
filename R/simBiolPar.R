#' The Atlantis-simulated focal species biological parameters in mskeyrun project
#'
#' Model name and scenario, Species code, species name, length weight, and other biological parameters
#'
#' @format A data frame
#'
#' \describe{
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{Code}{Atlantis model three letter code for functional group}
#'\item{Name}{Atlantis model common name for functional group}
#'\item{WLa}{Weight-Length equation parameter a, W = aL^b}
#'\item{WLb}{Weight-Length equation parameter b, W = aL^b}
#'\item{MinPreyWtProp}{Lower limit of prey/predator weight ratio}
#'\item{MaxPreyWtProp}{Upper limit of prey/predator weight ratio}
#'\item{SpawnMonth}{Spawning month of year converted from Atlantis time_spawn day of year}
#'\item{RecruitMonth}{Recruits month of arrival in model converted from Atlantis time_spawn + recruit_time day of year; if >12 recruitment is following year from SpawnMonth}
#'\item{AgeperAgecl}{Number of annual ages per Atlantis age class}
#'\item{NAgecl}{Number of Atlantis age classes}
#'\item{propMatAgecl1}{Proportion mature in Atlantis age class 1}
#'\item{propMatAgecl2}{Proportion mature in Atlantis age class 2}
#'\item{propMatAgecl3}{Proportion mature in Atlantis age class 3}
#'\item{propMatAgecl4}{Proportion mature in Atlantis age class 4}
#'\item{propMatAgecl5}{Proportion mature in Atlantis age class 5}
#'\item{propMatAgecl6}{Proportion mature in Atlantis age class 6}
#'\item{propMatAgecl7}{Proportion mature in Atlantis age class 7}
#'\item{propMatAgecl8}{Proportion mature in Atlantis age class 8}
#'\item{propMatAgecl9}{Proportion mature in Atlantis age class 9}
#'\item{propMatAgecl10}{Proportion mature in Atlantis age class 10}

#' }
#'
#'
"simBiolPar"
