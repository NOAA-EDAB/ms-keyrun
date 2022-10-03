#' Get fisheries length sample size by year
#'
#'

get_fishery_length_sample_n <- function(overwrite=F) {

  # identifies itis codes for all 10 species
  itis <- mskeyrun::focalSpecies %>%
    dplyr::distinct(SPECIES_ITIS) %>%
    dplyr::filter(!(SPECIES_ITIS %in% c(164727,166774))) %>%
    dplyr::pull()

  maintab <- NULL
  for(species in itis) {

    # herring and skates data came from maine and survey respectively
    # These datasets are not exported with mscatch
    db <- eval(rlang::parse_expr(paste0("mscatch::sampleLengths_",species,"_GB")))

    tab <- mskeyrun::focalSpecies %>%
      dplyr::select(SPECIES_ITIS,SVSPP,LongName) %>%
      dplyr::filter(SPECIES_ITIS == species) %>%
      dplyr::distinct()


    sptab <- db %>%
      dplyr::group_by(YEAR) %>%
      dplyr::summarise(lensampsize = as.double(sum(NUMLEN)),
                       ntrips = length(unique(tripid)),
                       .groups = "drop") %>%
      dplyr::mutate(YEAR= as.double(YEAR),
                    SPECIES_ITIS = as.double(tab$SPECIES_ITIS),
                    SVSPP = tab$SVSPP,
                    LongName = tab$LongName) %>%
      dplyr::select(YEAR,SVSPP,lensampsize,ntrips,SPECIES_ITIS,LongName)

    maintab <- rbind(maintab,sptab)

  }

  fisheryLenSampN <- maintab

  if (overwrite) {
    save(fisheryLenSampN,file=here::here("data/fisheryLenSampN.rda"))
  }

  return(fisheryLenSampN)

}
