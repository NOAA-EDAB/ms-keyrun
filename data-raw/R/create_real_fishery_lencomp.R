#'Read and format output from mscatch to mskeyrun
#'
#'same format as: mskeyrun::mskeyrun::simFisheryLencomp
#'
#'
#'
#'
#'
library(magrittr)

# filter its
itis <- mskeyrun::focalSpecies %>% 
  dplyr::select(SPECIES_ITIS,modelName) %>% 
  dplyr::filter(!(modelName == "Pollock")) %>%
  dplyr::distinct() %>% 
  dplyr::pull(SPECIES_ITIS)


realFisheryLencomp <- NULL

for (it in itis) {
  fileNm <- here::here("data-raw/data",paste0(it,".rds"))
  if (it %in% c(564145)) {
    next
  }
  
  if(file.exists(fileNm)) {
    message(paste0("Reading itis = ",it))
    speciesData <- readRDS(fileNm) # files in mscatch
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
    dplyr::summarise(value = sum(weight),.groups="drop") %>%
    dplyr::rename(year=YEAR,Code=species_itis,fishery=NEGEAR,lenbin = LENGTH) %>%
    dplyr::mutate(Name = name,ModSim = "Actual",variable = "biomass",units="metric tons") %>%
    dplyr::select(ModSim,year,Code,Name,fishery,lenbin,variable,value,units)
  
  
  realFisheryLencomp <- rbind(realFisheryLencomp,speciesFormat)
  
}
