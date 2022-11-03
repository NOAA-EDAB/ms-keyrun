#' Length comps
#'
#'Read and format output from mscatch to mskeyrun
#'
#'same format as: mskeyrun::mskeyrun::simFisheryLencomp
#'
#'
#'

library(magrittr)

create_real_fishery_lencomp_raw <- function(convertKGtoMT=T,outPath=NULL) {

  # 1kg = 2.2046226218 lbs


  if (convertKGtoMT  == T) {
    scalar <- 1000
    unitsLabel <- "metric tons"
  } else {
    scalar <- 1
    unitsLabel <- "kilograms"
  }


  itis <- mscatch::speciesLookupTable %>%
    dplyr::distinct(SPECIES_ITIS) %>%
    dplyr::filter(!(SPECIES_ITIS %in% c(164727,166774))) %>%
    dplyr::pull()

  realFisheryLencompRaw <- NULL

  for (it in itis) {
    # expandedLandings in kgs
    fileNmLen <- here::here("data-raw/data/",paste0("expandedLandingsRaw",it,".rds"))

    message(paste0("Reading itis = ",it))
    speciesDataLen <- readRDS(fileNmLen)

    name <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == it) %>%
      dplyr::pull(COMMON_NAME.y) %>%
      unique() %>%
      abutils::capitalize_first_letter()
    ## process data
    #ModSim        year Code  Name         fishery lenbin variable value units

      speciesFormat <- speciesDataLen %>%
        dplyr::group_by(YEAR,NEGEAR,LENGTH,species_itis) %>%
        dplyr::summarise(biomass = sum(weight)/scalar,
                         abundance = sum(numbers),
                         .groups="drop") %>%
        dplyr::rename(year=YEAR,Code=species_itis,fishery=NEGEAR,lenbin = LENGTH) %>%
        dplyr::mutate(Name = name,
                      ModSim = "Actual") %>%
        tidyr::pivot_longer(.,cols=c(biomass,abundance),
                            names_to = "variable",
                            values_to = "value") %>%
        dplyr::mutate(units = dplyr::case_when(variable == "biomass" ~ unitsLabel,
                                               variable == "abundance" ~ "numbers")) %>%
        dplyr::select(ModSim,year,Code,Name,fishery,lenbin,variable,value,units)


    realFisheryLencompRaw <- rbind(realFisheryLencompRaw,speciesFormat)


  }

  realFisheryLencompRaw <- realFisheryLencompRaw %>%
    dplyr::mutate(Code = as.double(Code))

  if (!is.null(outPath)) {
    save(realFisheryLencompRaw,file=outPath)
  }


  return(realFisheryLencompRaw)
}




