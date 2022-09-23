#' Length comps
#'
#'Read and format output from mscatch to mskeyrun
#'
#'same format as: mskeyrun::mskeyrun::simFisheryLencomp
#'
#'
#'
#'
library(magrittr)

create_real_fishery_lencomp <- function(convertKGtoMT=T,outPath=NULL) {

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

  realFisheryLencomp <- NULL

  for (it in itis) {
    # expandedLandings in kgs
    fileNm <- here::here("data-raw/data/",paste0("expandedLandings",it,".rds"))

    if(file.exists(fileNm)) {
      message(paste0("Reading itis = ",it))
      speciesData <- readRDS(fileNm)
    } else {
      message(paste0("Couldn't find itis = ",it))
      next
    }

    name <- mscatch::speciesLookupTable %>%
      dplyr::filter(SPECIES_ITIS == it) %>%
      dplyr::pull(COMMON_NAME.y) %>%
      unique() %>%
      abutils::capitalize_first_letter()
    ## process data
    #ModSim        year Code  Name         fishery lenbin variable value units
    speciesFormat <- speciesData %>%
      dplyr::group_by(YEAR,NEGEAR,LENGTH,species_itis) %>%
      dplyr::summarise(value = sum(weight)/scalar,.groups="drop") %>%
      dplyr::rename(year=YEAR,Code=species_itis,fishery=NEGEAR,lenbin = LENGTH) %>%
      dplyr::mutate(Name = name,
                    ModSim = "Actual",
                    variable = "biomass",
                    units=unitsLabel) %>%
      dplyr::select(ModSim,year,Code,Name,fishery,lenbin,variable,value,units)


    realFisheryLencomp <- rbind(realFisheryLencomp,speciesFormat)


  }
  realFisheryLencomp <- realFisheryLencomp %>%
    dplyr::mutate(Code = as.double(Code))

  if (!is.null(outPath)) {
    save(realFisheryLencomp,file=outPath)
  }


  return(realFisheryLencomp)
}




