#' Sarah's notes for building simulated dataset
#' 
#' all atlantis files are local on my computer in folder
#' ms-keyrun/simlulated-data/atlantisoutput
#' 
#' see SimData.Rmd for how these are generated using atlantisom
#' 
#' to make data for package
#' source these files in data-raw/R:
#'
#' create_sim_focal_species.R
#' get_sim_survey_index.R
#' 
#' run from ms-keyrun directory

library(here)

atlmod <- here("data-raw/simulated-data/config/NOBA_sacc38Config.R")

create_sim_focal_species(atlmod)

create_sim_biolpar(atlmod)

create_sim_survey_info(atlmod)

create_sim_survey_index(atlmod, fitstart=40, fitend=120)

create_sim_fishery_index(atlmod, fitstart=40, fitend=120)

create_sim_survey_agecomp(atlmod, fitstart=40, fitend=120)

create_sim_fishery_agecomp(atlmod, fitstart=40, fitend=120)

create_sim_survey_lencomp(atlmod, fitstart=40, fitend=120)

create_sim_fishery_lencomp(atlmod, fitstart=40, fitend=120)

create_sim_survey_dietcomp(atlmod, fitstart=40, fitend=120)

create_sim_survey_bottemp(atlmod, fitstart=40, fitend=120)

create_sim_fishery_wtage(atlmod, fitstart=40, fitend=120)

create_sim_survey_wtage(atlmod, fitstart=40, fitend=120)

create_sim_survey_agelen(atlmod, fitstart=40, fitend=120)

create_sim_percapconsumption(atlmod, fitstart=40, fitend=120)

create_sim_startpars(atlmod, fitstart=40, fitend=120)

