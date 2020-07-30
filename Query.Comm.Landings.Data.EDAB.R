#######
# Create time series of commercial landings from 1964-lyr using wolands, wodets and cfdets
#######



rm(list=ls())
ls()


source("C:/Users/kiersten.curti/Documents/R/Oracle_User_Data.R")
library(RODBC)
sole <- odbcConnect("sole",uid=user.name,pwd=password,believeNRows=FALSE)
rm(password)


lyr <- 2019

species <- c('212')
names(species) <- c('Mack')
#212 = Atlantic mackerel


land.dir <- 'C:/Users/kiersten.curti/Desktop/Work/EDAB'  



############################################################################################



##### Years #####

# CFDETS/CFLEN
cfdets.yrs <- 1994:lyr
cfdets.nyr <- length(cfdets.yrs)

# WODETS
wodets.yrs <- 82:93
  names(wodets.yrs) <- paste('19',wodets.yrs,sep="")
wodets.nyr <- length(wodets.yrs)

# WOLANDS
wolands.yrs <- 64:81
  names(wolands.yrs) <- paste('19',wolands.yrs,sep="")
wolands.nyr <- length(wolands.yrs)



##### Stratification variables #####


# State abbreviations
states <- sqlQuery(sole, "select unique(statecd), stateabb from port;")
colnames(states) <- c("STATE","STATEABB")
  

# Function to assign stratification categories
assign.cats <- function(x)  {

  # Assign time categories:  quarter and semester
  if(dataset%in%c('cfdets','wodets','wolands','woage'))  {
    # quarter
    x[x$MONTH %in% c(1:3),'QTR'] <- 1
    x[x$MONTH %in% c(4:6),'QTR'] <- 2
    x[x$MONTH %in% c(7:9),'QTR'] <- 3
    x[x$MONTH %in% c(10:12),'QTR'] <- 4
    x[x$MONTH %in% c(0),'QTR'] <- 0
    }
  # semester
  x[x$MONTH %in% c(1:6),'SEMESTER']  <- 1
  x[x$MONTH %in% c(7:12),'SEMESTER'] <- 2
  x[x$MONTH %in% c(0),'SEMESTER']  <- 0

  # Assign area fished
  x[is.na(x$AREA)==TRUE,'AREA'] <- -99999
  x[x$AREA>=600,'AREAF'] <- 'MA'
  x[x$AREA<600,'AREAF'] <- 'NE'
  x[x$AREA<464,'AREAF'] <- 'Less464'
  x[x$AREA==0,'AREAF'] <- 'Zero'
  x[x$AREA=='-99999','AREAF'] <- 'OrigNA'
  x[x$AREA>='700','AREAF'] <- 'Greater700'
  # Return NA label to area from -99999
  x[x$AREA=='-99999','AREA'] <- NA
  

  # Merge with state names
  x2 <- merge(x, states, all.x=TRUE, all.y=FALSE)

  } # end assign.cats function



############################### LANDINGS ###############################



##### CFDETS #####
dataset <- 'cfdets'

cfdets <- vector('list',cfdets.nyr)
  names(cfdets) <- cfdets.yrs

for (year in cfdets.yrs)  {  
  # year <- cfdets.yrs[5]

  cfdets.qry <- paste("select year, month, negear, negear2, area, nespp4, source, port, state, utilcd, sum(spplivlb) slbs from ",dataset,year,"AA"," where nespp3 in ('",species,"') group by year, month, negear, negear2, area, nespp4, source, port, state, utilcd", sep="")
  cfdets.res <- sqlQuery(sole, cfdets.qry)
  cfdets.res$'MT' <- cfdets.res$SLBS/2204.62262
  
  # Assign stratification variables and add to list
  cfdets[[as.character(year)]] <- assign.cats(cfdets.res)
  }  # end of cfdets loop



  
##### WODETS #####

wodets <- vector('list',wodets.nyr)
  names(wodets) <- names(wodets.yrs)


for (t in 1:wodets.nyr)  {  
  year <- wodets.yrs[t]
  year4<- names(wodets.yrs)[t]
    
  wodets.qry <- paste("select sp.year, sp.month, sp.day, sp.vessel, sp.link, sp.negear, sp.negear2, sp.area, trip.intv, sp.nespp4, sp.port, sp.state, sp.utilcd, sum(spplivlb) slbs FROM wodets",year," sp LEFT JOIN wodett",year," trip ON  sp.year=trip.year AND sp.month=trip.month AND sp.day=trip.day AND sp.vessel=trip.vessel AND sp.link=trip.link AND sp.negear=trip.negear AND sp.area=trip.area WHERE sp.nespp3='", species, "' group by sp.year, sp.month, sp.day, sp.vessel, sp.link, sp.negear, sp.negear2, sp.area, trip.intv, sp.nespp4, sp.port, sp.state, sp.utilcd;",sep="")

  wodets.res <- sqlQuery(sole, wodets.qry, stringsAsFactors=FALSE)
  wodets.res$'MT' <- wodets.res$SLBS/2204.62262

  # Modify year to be 4 digits
  wodets.res$YEAR <- as.numeric(paste('19',wodets.res$YEAR,sep=""))

  # Modify colnames
  cnames <- colnames(wodets.res)
  cnames[cnames=='INTV'] <- 'SOURCE'
  colnames(wodets.res) <- cnames
  # Drop variables
  cnames.abb <- cnames[!(cnames%in%c('DAY','VESSEL','LINK'))]
  wodets.abb <- wodets.res[,cnames.abb]

  # Assign stratification variables and add to list
  wodets[[year4]] <- assign.cats(wodets.abb)  
 }  # end of wodets/wodett loop


  

##### WOLANDS #####
dataset <- 'wolands'

wolands <- vector('list',wolands.nyr)
  names(wolands) <- names(wolands.yrs)

for (t in 1:wolands.nyr)  {  
  year <- wolands.yrs[t]
  year4<- names(wolands.yrs)[t]
    
  wolands.qry <- paste("select year, month, negear, negear2, area, nespp4, intv, port, state, utilcd, sum(spplivlb) slbs from ",dataset,year," where nespp3 in ('",species,"') group by year, month, negear, negear2, area, nespp4, intv, port, state, utilcd", sep="")
  wolands.res <- sqlQuery(sole, wolands.qry) 
  wolands.res$'MT' <- wolands.res$SLBS/2204.62262
  cnames <- colnames(wolands.res)
  cnames[cnames=='INTV'] <- 'SOURCE'
  colnames(wolands.res) <- cnames
    
  # Modify year to be 4 digits
  wolands.res$YEAR <- as.numeric(paste('19',wolands.res$YEAR,sep=""))
  
  # Assign stratification variables and add to list
  wolands[[year4]] <- assign.cats(wolands.res) 
  }  # end of wolands loop




##### Concatenate #####

head(wolands[[wolands.nyr]])
head(wodets[[wodets.nyr]])
head(cfdets[[cfdets.nyr]])

# Concatenate lists into matrices
cfdets.mat <- do.call(rbind,cfdets)
  rownames(cfdets.mat) <- NULL
wodets.mat <- do.call(rbind,wodets)
  rownames(wodets.mat) <- NULL
wolands.mat <- do.call(rbind,wolands)
  rownames(wolands.mat) <- NULL

land.all <- c(wolands, wodets ,cfdets)
land.all.mat <- rbind(wolands.mat, wodets.mat, cfdets.mat)
  head(land.all.mat)
  dim(land.all.mat)


  

### Organize by area and areaf

  
area.list <- sort( unique(land.all.mat$AREA), na.last=FALSE )
  
land.area <- lapply(land.all, function(x) {
  # x <- land.all[[1]]
  area.vec <- factor(x$AREA, levels=area.list) 
  x1 <- tapply(x$MT, area.vec, sum)
  x1[is.na(x1)==TRUE] <- 0
  x1
  } )
land.area.prop <- lapply(land.area, function(x) {x/sum(x);} )
land.area.mat <- t(do.call(rbind,land.area))
land.area.prop.mat <- t(do.call(rbind,land.area.prop)) 

  
  
areaf.order <-  c("NE", "MA", "Less464", "Greater700", "Zero")

land.areaf <- lapply(land.all, function(x) {
  x1 <- tapply(x$MT, x$AREAF, sum)
  x1 <- x1[areaf.order]
  names(x1) <- areaf.order
  x1[is.na(x1)==TRUE] <- 0
  x1
} )
land.areaf.prop <- lapply(land.areaf, function(x) {x/sum(x);} )
land.areaf.mat <- t(do.call(rbind,land.areaf))
land.areaf.prop.mat <- t(do.call(rbind,land.areaf.prop)) 



############################### SAVE ###############################



rm(cfdets.res, cnames, cnames.abb, dataset, t, wodets.abb, wodets.res, wolands.res, year, year4)


setwd(land.dir)
save.image(file.path(land.dir, paste(names(species),'Landings.through',lyr,"Rdata",sep=".")))



