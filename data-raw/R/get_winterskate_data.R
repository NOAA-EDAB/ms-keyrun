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

get_winterskate_data <- function(channel) {

  skate <- list()
  #Pulling length data
  message("Pulling Winter skate length svdbs ...")

  skate <- mscatch:::get_length_weight_age_data(channel,year="all",species=23)

  saveRDS(skate,here::here("Data-raw/data","lengthWeight23.rds"))

  return(skate)

}
