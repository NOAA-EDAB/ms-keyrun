#Pull diet data
library(dbutils); library(data.table); library(here)

channel <- dbutils::connect_to_database(server="sole",uid="slucey")

allfh.qry <- "select year, season, cruise6, station, stratum, tow, declat, declon,
             svspp, pdid, pdgutw, pdgutv,
             pynam, pyamtw, pyamtv, perpyw, perpyv
             from FHDBS.ALLFH_FEAST"

allfh <- data.table::as.data.table(DBI::dbGetQuery(channel, allfh.qry))

#SOE Strata sets
MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
SS  <- c(1300:1352, 3840:3990)

for(iepu in c('MAB', 'GB', 'GOM', 'SS')){
  strata.set <- get(iepu)
  allfh[as.numeric(STRATUM) %in% strata.set, EPU := iepu][]
}

GB.fh  <- allfh[EPU == 'GB'  & !PYNAM %in% c('EMPTY', 'BLOWN'), ]
GOM.fh <- allfh[EPU == 'GOM' & !PYNAM %in% c('EMPTY', 'BLOWN'), ]
MAB.fh <- allfh[EPU == 'MAB' & !PYNAM %in% c('EMPTY', 'BLOWN'), ]

#Save for Stony Brook Students
#save(GOM.fh, file = here('data/GOM_foodhabits.RData'))
#save(MAB.fh, file = here('data/MAB_foodhabits.RData'))
