#Get raw survey data
library(here); library(data.table); library(survdat)

spp <- c(32, 73, 197, 74, 15, 106, 105, 121, 72, 23, 75)

#Connect to database
channel <- dbutils::connect_to_database(server = 'sole', uid = 'slucey')

#Pull data
data <- get_survdat_data(channel)

ms.data <- data$survdat[SVSPP %in% spp, ]

#save data
