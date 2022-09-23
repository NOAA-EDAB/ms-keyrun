#' Processes herring data
#'
#'Herring Data comes from the state of Maine.
#'
#'@param channel DBI object. connection object for database access
#'
#'@return Processed Herring data added to comland
#'
#'@importFrom data.table ":=" "key"
#'
#' @noRd
#' @export

get_herring_data <- function(channel) {

  herring <- list()
  #Pulling data
  message("Pulling Atlantic herring data from maine_herring_catch ...")

  herr.qry <- "select year, month, stock_area, negear, gearname, keptmt, discmt
                      from maine_herring_catch"

  herr.catch <- data.table::as.data.table(DBI::dbGetQuery(channel, herr.qry))

  # get length data
  # samplength in mm
  # sampweight in mt
  herr.len.qry <- "select year, month, negear, gearname, stock_area, frequency,
                      age, samplength, sampweight, sex
                      from cfdbs.maine_herring_samp;"
  herr.len <- data.table::as.data.table(DBI::dbGetQuery(channel, herr.len.qry))

  herring$catch <- herr.catch
  herring$length <- herr.len
  saveRDS(herring,here::here("herringData.rds"))

   return(herring)

}
