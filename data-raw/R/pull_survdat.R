#Pull survey data - require you to be behind the NEFSC firewall
library(here); library(data.table); library(survdat)

#Connect to database
channel <- dbutils::connect_to_database(server = 'sole', uid = 'slucey')

#Pull data
data <- survdat::get_survdat_data(channel)

#save data
saveRDS(data, file = here::here('data-raw', 'survdat.RDS'))

#Pull bio data
bio <- survdat::get_survdat_data(channel, getBio = T)

saveRDS(bio, file = here::here('data-raw', 'survdat_bio.RDS'))