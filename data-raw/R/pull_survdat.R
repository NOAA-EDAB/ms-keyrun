#Pull survey data - require you to be behind the NEFSC firewall
library(here); library(data.table); library(survdat)

#Refer to Data decisions Nov. 2020
#Common temporal footprint of 1968 - 2019 for Spring and Fall survey
#Separate data pulls for Albatross (A4) and Bigelow (HB)
#Will eventually use SOE survey footprint

#Connect to database
channel <- dbutils::connect_to_database(server = 'sole', uid = 'slucey')

#Pull data
survdat.a4 <- survdat::get_survdat_data(channel, filterByYear = 1968:2008, 
                                        conversion.factor = T)
survdat.hb <- survdat::get_survdat_data(channel, filterByYear = 2009:2019,
                                        conversion.factor = F)
survdat    <- survdat::get_survdat_data(channel, filterByYear = 1968:2019,
                                        conversion.factor = T)

#save data
save(survdat.a4, file = here::here('data-raw', 'data', 'survdat_Albatross.RData'))
save(survdat.hb, file = here::here('data-raw', 'data', 'survdat_Bigelow.RData'))
save(survdat,    file = here::here('data-raw', 'data', 'survdat.RData'))

#Pull bio data
survdat.bio <- survdat::get_survdat_data(channel, getBio = T)

save(survdat.bio, file = here::here('data-raw', 'data', 'survdat_bio.RData'))