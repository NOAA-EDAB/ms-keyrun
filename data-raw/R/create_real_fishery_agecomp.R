#'Age Comps
#'
#'Read and format output from mscatch to mskeyrun
#'
#'
#'same format as: mskeyrun::mskeyrun::simFisheryAgecomp
#'
#'
#'
#'
#'
#'
#'   ModSim        year Code  Name         fishery   age variable value units
#    <chr>        <dbl> <chr> <chr>        <chr>   <dbl> <chr>    <int> <chr>
#  1 NOBA_sacc_38    55 BWH   Blue_whiting census      1 Natage     124 number
#  2 NOBA_sacc_38    55 BWH   Blue_whiting census      2 Natage     198 number
#'
library(magrittr)

create_real_fishery_agecomp <- function(outPath=NULL){


  itis <- mscatch::speciesLookupTable %>%
    dplyr::distinct(SPECIES_ITIS) %>%
    dplyr::filter(!(SPECIES_ITIS %in% c(164727,166774))) %>%
    dplyr::pull()

  realFisheryAgecomp <- NULL


  for (it in itis) {
    fileNm <- here::here("data-raw/data",paste0("numbersatAge",it,".rds"))
  #  if (it %in% c(161722)) {
  #    next
  #  }

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

    # pivot table to have age in own column

   speciesData <- speciesData %>%
     dplyr::select(-c(TIME,MARKET_CODE,LENGTH,NUMLEN,weight,fishWeight,numbers)) %>%
     tidyr::pivot_longer(.,cols=dplyr::starts_with("AGE_",ignore.case=F),names_to = "age",values_to = "numbers") %>%
     tidyr::separate(.,col="age",into = c(NA,"newage"),sep="_") %>%
     dplyr::rename(age = newage)

    ## process data
    speciesFormat <- speciesData %>%
      dplyr::group_by(YEAR,NEGEAR,age,species_itis) %>%
      dplyr::summarise(value = sum(numbers),.groups="drop") %>%
      dplyr::rename(year=YEAR,Code=species_itis,fishery=NEGEAR) %>%
      dplyr::mutate(Name = name,
                    ModSim = "Actual",
                    variable = "Natage",
                    units="number") %>%
      dplyr::select(ModSim,year,Code,Name,fishery,age,variable,value,units)


    realFisheryAgecomp <- rbind(realFisheryAgecomp,speciesFormat)


  }


  realFisheryAgecomp <- realFisheryAgecomp %>%
    dplyr::mutate(Code = as.double(Code),
                  year = as.double(year),
                  age = as.double(age))

  if (!is.null(outPath)) {
    save(realFisheryAgecomp,file=outPath)
  }


  return(realFisheryAgecomp)

}
