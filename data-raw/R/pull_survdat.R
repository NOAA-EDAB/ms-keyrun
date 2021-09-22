#Pull survey data - require you to be behind the NEFSC firewall
library(here); library(data.table); library(survdat)

#Connect to database
channel <- dbutils::connect_to_database(server = 'sole', uid = 'slucey')

#Pull data
survdat <- survdat::get_survdat_data(channel)

#save data
save(survdat, file = here::here('data-raw', 'survdat.RData'))

#Pull bio data
survdat.bio <- survdat::get_survdat_data(channel, getBio = T)

save(survdat.bio, file = here::here('data-raw', 'survdat_bio.RData'))