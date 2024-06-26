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

create_sim_fishery_index(atlmod, fitstart=40, fitend=120) #creates subannual amd aggregate

create_sim_survey_agecomp(atlmod, fitstart=40, fitend=120)

create_sim_fishery_agecomp(atlmod, fitstart=40, fitend=120)

create_sim_fishery_agecomp_subannual(atlmod, fitstart=40, fitend=120)

create_sim_survey_lencomp(atlmod, fitstart=40, fitend=120)

create_sim_fishery_lencomp(atlmod, fitstart=40, fitend=120)

create_sim_fishery_lencomp_subannual(atlmod, fitstart=40, fitend=120)

create_sim_survey_dietcomp(atlmod, fitstart=40, fitend=120)

create_sim_survey_bottemp(atlmod, fitstart=40, fitend=120)

create_sim_fishery_wtage(atlmod, fitstart=40, fitend=120)

create_sim_fishery_wtage_subannual(atlmod, fitstart=40, fitend=120)

create_sim_survey_wtage(atlmod, fitstart=40, fitend=120)

create_sim_survey_agelen(atlmod, fitstart=40, fitend=120)

create_sim_fishery_agelen(atlmod, fitstart=40, fitend=120)

create_sim_percapconsumption(atlmod, fitstart=40, fitend=120)

create_sim_startpars(atlmod, fitstart=40, fitend=120)

# food web model specific datasets add other species

create_sim_survey_index_fw(atlmod, fitstart=40, fitend=120)

create_sim_fishery_index_fw(atlmod, fitstart=40, fitend=120)

create_sim_survey_dietcomp_fw(atlmod, fitstart=40, fitend=120)

# below combines already loaded mskeyrun datasets,  
# outputs of create_sim_survey_agelen and create_sim_survey_dietcomp
# ensure that these are up to date before running

create_sim_survey_lendietcomp()


