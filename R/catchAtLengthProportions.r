#' catch by size class, area, fleet
#'
#' For the species found in the Hydra model, the \href{https://noaa-edab.github.io/ms-keyrun/articles/GBLandingsByLength.html}{proportion} 
#' of total landings are reported by \href{https://noaa-edab.github.io/ms-keyrun/articles/GBFleetDefinitions.html}{fleet}, year, and 
#' by \href{https://noaa-edab.github.io/ms-keyrun/articles/GBLandingsByLength.html}{sizeclass}
#'
#' @format A data frame with 3 columns
#'
#' \describe{
#'   \item{fleet}{Name of \href{https://noaa-edab.github.io/ms-keyrun/articles/GBFleetDefinitions.html}{fleet}}
#'   \item{area}{Area in which landings are reported}
#'   \item{year}{year in which landings reported}
#'   \item{species_itis}{itis code}
#'   \item{type}{type of data. 0 = landings}
#'   \item{totalBiomass}{Total Biomass}
#'   \item{sizeclass1}{proportion of total biomass attributed to size class 1 }
#'   \item{sizeclass2}{proportion of total biomass attributed to size class 1}
#'   \item{sizeclass3}{proportion of total biomass attributed to size class 1}
#'   \item{sizeclass4}{proportion of total biomass attributed to size class 1}
#'   \item{sizeclass5}{proportion of total biomass attributed to size class 1}
#'
#' }
#'
#'
"catchAtLengthProportions"
