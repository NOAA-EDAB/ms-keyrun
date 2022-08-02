#Pull landings data - require you to be behind the NEFSC firewall
library(here); library(data.table); library(comlandr)

#Refer to Data decisions Nov. 2020
#Common temporal footprint of 1968 - 2019 

#Connect to database
channel <- dbutils::connect_to_database(server = 'sole', uid = 'slucey')

#Pull data
comland <- comlandr::get_comland_data(channel, filterByArea = c(521, 522, 525, 
                                                                526, 537, 538, 
                                                                551, 552, 561,
                                                                562), 
                                      aggArea = T, aggGear = T,
                                      unkVar = c('MONTH', 'Fleet', 'EPU'),
                                      knStrata = c('NESPP3', 'YEAR', 'EPU',
                                                   'HY', 'QY', 'MONTH', 
                                                   'Fleet', 'TONCL1'))
comland$comland <- comland$comland[EPU == 'GB', ]

#save data
save(comland, file = here::here('data-raw', 'data', 'comland.RData'))
