#Calculate proportions of commercial landings on Georges Bank
library(here); library(data.table); library(magrittr)

vtr <- as.data.table(readRDS(here('/data-raw/data/Landings_VTR_Geret_Data.rds')))
  
# split the are column to define inside and out, sum landings by year, spp, area and 
# select just 12 species that make up 90% of landings
props <- vtr %>%
  dplyr::select(Year,Area,NESPP3,InsideLANDED) %>% 
  dplyr::filter(!Area == "Other") %>%
  dplyr::mutate(AREA=stringr::str_split_fixed(Area,"_",2)[,1]) %>%
  dplyr::mutate(InOut=stringr::str_split_fixed(Area,"_",2)[,2]) %>%
  dplyr::mutate(YEAR = as.numeric(Year)) %>% 
  dplyr::mutate(AREA = as.numeric(AREA)) %>% 
  dplyr::group_by(YEAR,AREA,NESPP3,InOut) %>%
  dplyr::summarise(TOTALLANDINGS=sum(InsideLANDED)) %>% 
  dplyr::ungroup() %>%
  data.table::as.data.table()

#Calculate proportions
props[, AllLandings := sum(TOTALLANDINGS), by = c('YEAR', 'AREA', 'NESPP3')]
#Remove areas that have no landings
props <- props[AllLandings != 0, ]
props[, Prop := TOTALLANDINGS / AllLandings]
props[, MeanProp := mean(Prop), by = c('AREA', 'NESPP3', 'InOut')]

props <- unique(props, by = c('AREA', 'NESPP3', 'InOut'))

#Drop extra columns
props[, c('YEAR', 'TOTALLANDINGS', 'AllLandings', 'Prop') := NULL]

AllSpeciesProportions <- props

#saveRDS(props, file = here::here('data-raw/data/All_Species_Proportions.rds'))
usethis::use_data(AllSpeciesProportions)
