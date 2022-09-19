#' Creates table for GBLandingsLength.rmd
#' 
#' reads in the output get_species_object_mskeyrun and forms a table to display
#' the inputs used to expand the landings to length and form the age-length key
#' 
#' @param nm Character string. Name of field in `get_species_object_mskeyrun` output. Either "gearcodes" or "marketCodes"
#' @param reuse Character string. Name of field to relabel the "use" column
#' @param recombine Character string. Name of field to relabel the "combine" column
#' 
#' @return flextable object

library(magrittr)


create_species_rules_table1 <- function(nm,reuse,recombine) {
  
  itis <- mskeyrun::focalSpecies %>%
    dplyr::distinct(SPECIES_ITIS) %>%
    dplyr::filter(!(SPECIES_ITIS %in% c(164727))) %>%
    dplyr::pull()
  
  maindf <- data.frame()
  
  for (iitis in 1:length(itis)) {
    spitis <- itis[iitis]
    speciesRules <- mscatch::get_species_object_mskeyrun(spitis)

    zz <- speciesRules[[nm]]
    zz$species <- speciesRules$speciesName
    
    zz <- zz %>% 
      dplyr::rename(!!reuse := use,
                    !!recombine := combine) %>%
      dplyr::select(species,!!reuse,!!recombine) 
    
    maindf <- rbind(maindf,zz)


  }
  
  tab <- maindf  %>%
    dplyr::group_by(species,.data[[reuse]]) %>%
    dplyr::summarize(temp=paste(sort(.data[[recombine]]),collapse = ","),.groups = "drop") %>% 
    dplyr::rename(!!recombine := temp) %>%
    flextable::flextable() %>%
    flextable::merge_v(j="species") %>%
    flextable::theme_vanilla()


  return(tab)
}


#' Creates table for GBLandingsLength.rmd
#' 
#' reads in the output get_species_object_mskeyrun and forms a table to display
#' the inputs used to expand the landings to length and form the age-length key
#' 
#' @return flextable object

create_species_rules_table2 <- function() {
  
  itis <- mskeyrun::focalSpecies %>%
    dplyr::distinct(SPECIES_ITIS) %>%
    dplyr::filter(!(SPECIES_ITIS %in% c(164727))) %>%
    dplyr::pull()
  
  maindf <- data.frame()
  
  for (iitis in 1:length(itis)) {
    spitis <- itis[iitis]

    speciesRules <- mscatch::get_species_object_mskeyrun(spitis)
    
    speciesRules$gearCodes <- NULL
    speciesRules$marketCodes <- NULL
    
    for (ifi in names(speciesRules)) {

      if (length(speciesRules[[ifi]]) > 1) {
        speciesRules[ifi] <- paste(speciesRules[[ifi]], collapse = ",")
      } 
    
    }
    zz <- as.data.frame(speciesRules)
    
    maindf <- rbind(maindf,zz)

  }
  
  stockAreaEqual <- maindf %>% 
    dplyr::select(statStockArea) %>% 
    dplyr::distinct() %>% 
    nrow() 
  
  if (stockAreaEqual == 1) {
    message(paste0("All species catch data are pulled from the statistical areas, ",speciesRules$statStockArea))
  }

  tab <- maindf  %>%
    dplyr::rename(species = speciesName) %>% 
    dplyr::select(-c(species_itis,SVSPP,statStockArea)) %>%
    flextable::flextable() %>%
    flextable::merge_v(j="species") %>%
    flextable::theme_vanilla()
  # 
  # 
  return(tab)
  #return(maindf)
}

