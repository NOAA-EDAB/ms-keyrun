#' sample Data pull of all species in Hydra model on GB
#'
#'Sample data in the format we'll need from proper data pulls using Seans comland script.
#'Landings pulled from "MV_CF_Landings", lengths pulled from "mv_cf_len"
#'
#'All missing areas need to dealt with prior to pulling species data.
#'Not the case in this sample. We pull from GB
#'
#'@param channel obtained from logging into the sole database.
#'
#'@section Other species of interest:
#'212 - atlantic mackerel - (K.Curti assessment)
#'081 - cod (mike palmer assessment)
#'

#channel <- cfdbs::connect_to_database("server","user")
source(here::here("data-raw","test_mscatch_data_pull.r"))

test_mscatch_data_pull_hydra_GB <- function(channel){

  library(magrittr)

  hydraSpecies <- data.frame(oldName=c("Acod","Aherring","Amackerel","goosefish","haddock",
                                       "silverhake","spinydog","winterfl","winterskate","yellowtailfl"),
                             newName=c("ATLANTIC COD","ATLANTIC HERRING","ATLANTIC MACKEREL","GOOSEFISH","HADDOCK",
                                       "SILVER HAKE","SPINY DOGFISH","WINTER FLOUNDER","WINTER SKATE","YELLOWTAIL FLOUNDER")) %>%
    # hydraSpecies <- data.frame(oldName=c("Aherring","Amackerel", "silverhake","spinydog","winterfl","winterskate"),
    #                            newName=c("ATLANTIC HERRING","ATLANTIC MACKEREL","SILVER HAKE","SPINY DOGFISH","WINTER FLOUNDER","WINTER SKATE")) %>%
    dplyr::left_join(.,mscatch::speciesLookupTable,by=c("newName"="COMMON_NAME.y")) %>%
    dplyr::select(oldName,newName,SPECIES_ITIS) %>%
    dplyr::distinct()

  GBArea <- c(cfdbs::EPUs$data$GB,537)

  for (itis in unique(hydraSpecies$SPECIES_ITIS)) {
    message(paste0("pulling: ",itis))
    #fileName <- paste0(itis,"_GB")
    test_mscatch_data_pull(channel,species=itis,species_itis = T, area = GBArea,areaLengths="all", stock="GB")
  }


}
