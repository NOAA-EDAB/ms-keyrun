#' Run cluster anlaysis
#'
#' Creates similarity matrix for passed data then runs cluster analysi
#'
#' @param gearTable Data frame. 
#'
#'
#'

cluster_gears <- function(gearTable) {
  

  # organize the data into wide data frame to calculate similarity matrix
  df <- gearTable %>%
    tidyr::pivot_wider(.,
                       id_cols=c(GEARID,NESPP3),
                       names_from = NESPP3,
                       values_from = totsplandlb)
  df[is.na(df)] <- 0
  df <- tibble::column_to_rownames(df,var="GEARID")
  
  # standardize columns
  for (icol in 1:ncol(df)) {
    mn <- mean(df[,icol])
    sdev <- sd(df[,icol])
    df[,icol] <- (df[,icol]-mn)/sdev
  }
  
  
  # Cluster analysis --------------------------------------------------------
  
  # similarity matrix (disimilarity)
  simMat <- cluster::daisy(df,metric="euclidean",stand = F)
  # hierarchical cluster analysis
  clusterObj <- cluster::agnes(simMat,diss=T)

  return(clusterObj)
}