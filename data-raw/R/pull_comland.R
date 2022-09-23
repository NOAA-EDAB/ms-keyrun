#Pull landings data - require you to be behind the NEFSC firewall
library(here); library(data.table); library(comlandr)

#Refer to Data decisions Nov. 2020
#Common temporal footprint of 1968 - 2019 

#Connect to database
channel <- dbutils::connect_to_database(server = 'sole', uid = 'slucey')

#Pull data
landings <- comlandr::get_comland_data(channel, filterByYear = 1968:2019,
                                       filterByArea = c(521, 522, 525, 526, 537, 
                                                        538, 551, 552, 561, 562), 
                                      aggArea = T, aggGear = T,
                                      unkVar = c('MONTH', 'Fleet', 'EPU'),
                                      knStrata = c('NESPP3', 'YEAR', 'EPU',
                                                   'HY', 'QY', 'MONTH', 
                                                   'Fleet', 'TONCL1'))

#Pull discard data
channel2 <- dbutils::connect_to_database('nova', 'slucey')

discards <- comlandr::get_comdisc_data(channel2, landings, aggArea = T, aggGear = T,
                                       extendTS = T)

#Filter to just Georges Bank
landings$comland <- landings$comland[EPU == 'GB', ]
discards$comdisc <- discards$comdisc[EPU == 'GB', ]

#save data
save(landings, file = here::here('data-raw', 'data', 'landings.RData'))
save(discards, file = here::here('data-raw', 'data', 'discards.RData'))
