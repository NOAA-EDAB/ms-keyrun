# build diet datasets for mskeyrun
# Sarah's notes
# source create_real_dietcomp.R and create_real_lendietcomp.R
# run the following

library(magrittr)

GBsurvstrata  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)

focalspp <- mskeyrun::focalSpecies %>%
  dplyr::filter(modelName != "Pollock") %>% # not using in these models
  dplyr::mutate(Name = modelName)

create_real_dietcomp(focalspp, GBsurvstrata)

create_real_lendietcomp(focalspp, GBsurvstrata)

