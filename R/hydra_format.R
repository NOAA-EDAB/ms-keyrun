#' Format expanded landings for Hydra
#'
#'
#' 1. Consolidate NEGEAR to NEGEAR2 (2 or 3 digit gear code use)
#' 2. Drop market category
#' 3. Find total Biomass by fleet and year for each species
#' 4. Within fleet and year attribute biomass of fish into length bins
#'   specified by hydra (hydradata::hydraDataList$binwidth)
#' @noRd



format_hydra <- function(expandedLandings,fleets,itis) {

  # truncate Gear code to 2 digit gear code
  new <- expandedLandings %>%
    dplyr::mutate(NEGEAR2 = base::substr(NEGEAR,start=1,stop=2)) %>%
    dplyr::select(-NEGEAR)

  dataNew <- new %>% dplyr::left_join(.,fleets,by=("NEGEAR2")) %>%
    dplyr::group_by(YEAR,species_itis,hydraFleets,LENGTH) %>%
    dplyr::summarise(totalWeight=sum(weight),.groups="drop")

  hydraSpecies <- data.frame(oldName=c("Acod","Aherring","Amackerel","goosefish","haddock",
                                       "silverhake","spinydog","winterfl","winterskate","yellowtailfl"),
                             newName=c("ATLANTIC COD","ATLANTIC HERRING","ATLANTIC MACKEREL","GOOSEFISH","HADDOCK",
                                       "SILVER HAKE","SPINY DOGFISH","WINTER FLOUNDER","WINTER SKATE","YELLOWTAIL FLOUNDER")) %>%
    dplyr::left_join(.,mscatch::speciesLookupTable,by=c("newName"="COMMON_NAME.y")) %>%
    dplyr::select(oldName,newName,SPECIES_ITIS) %>%
    dplyr::distinct()

  # organizes hydra length bins into long format table, change species name format , add itis
  modbins <- hydradata::hydraDataList$binwidth %>%
    dplyr::mutate(species = rownames(.)) %>%
    tidyr::pivot_longer(!species, names_to = "sizebin", values_to = "binwidth") %>%
    dplyr::group_by(species) %>%
    dplyr::arrange(sizebin) %>%
    dplyr::mutate(min = cumsum(binwidth)-binwidth,
                  max = cumsum(binwidth)) %>%
    dplyr::arrange(species, sizebin) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(.,hydraSpecies,by=c("species"="oldName")) %>%
    dplyr::select(-species) %>%
    dplyr::rename(species = newName) %>%
    dplyr::select(sizebin,binwidth,min,max, species,SPECIES_ITIS)

  # for each species aggregate weight by length into bins as required by hydra

    message(itis)
    bins <- modbins %>% dplyr::filter(SPECIES_ITIS==itis) %>% dplyr::pull(max)
    # grab species length data and aggregate into length bins
    spdkeep <- dataNew %>%
      dplyr::filter(species_itis == itis) %>%
      dplyr::group_by(YEAR,hydraFleets,bins = cut(LENGTH,breaks=c(0,bins),right=T)) %>%
      dplyr::summarize(w = sum(totalWeight),.groups="drop")

    spd <- dataNew %>%
      dplyr::filter(species_itis == itis) %>%
      dplyr::group_by(YEAR,hydraFleets,bins = cut(LENGTH,breaks=c(0,bins),right=T,include_lowest=T,
                                                  labels=paste0("sizeclass",1:length(bins)))) %>%
      dplyr::summarize(w = sum(totalWeight),.groups="drop")

    # for all fleets calculate the proportions of landings in each size bin
    # remove other fleet and landings witout length bins.
    # These dealt with separately
    proportionsinit <- spd %>% dplyr::group_by(YEAR,hydraFleets) %>%
      dplyr::filter(hydraFleets != "other") %>%
      dplyr::filter(!is.na(bins)) %>%
      dplyr::mutate(prop = w/sum(w))

    # now we need to deal with "other" gear. (lump in most abundant gear type). If have lengths combine to lengths of most abundant
    # and deal with gear type landings with no lengths eg. lobster gear that catch flounder have no length data
    # for these we will calculate proportion excluding this, then add landings to total. (Assumes distribution among length is same)

    # find gear type dominating landings
    dominantFleetByYear <- spd %>% dplyr::group_by(YEAR,hydraFleets) %>%
      dplyr::summarise(binTot=sum(w),.groups="drop") %>%
      dplyr::group_by(YEAR) %>%
      dplyr::filter(binTot==max(binTot))

    # now deal with other gear. Combine landings with lengths
    # find other fleet with length bin data. add to dominant gear
    sss <- spd
    dominantFleetByYear2 <- dominantFleetByYear
    # loop over years
    for (iy in unique(spd$YEAR)) {
      #print(iy)
      otherFleet <- spd %>%
        dplyr::filter(YEAR == iy) %>%
        dplyr::filter(hydraFleets == "other")

      if (nrow(otherFleet) < 1){ # if no other year for year
        next
      }

      for (irow in 1:nrow(otherFleet)){
        dominantFleet <- dominantFleetByYear %>%
          dplyr::filter(YEAR == iy) %>%
          dplyr::pull(hydraFleets)


        if (is.na(otherFleet$bins[irow])) {
          # no bin data. Add weight to dominant gears total weight (binTot)
          #print("other - NA")

        } else {
          #print("bins")
          # match length bins and add to dominant gear length bin
          index <- (spd$YEAR==iy) * (spd$hydraFleets == dominantFleet) * (spd$bins == otherFleet$bins[irow])
          index[is.na(index)] <- 0
          index <- as.logical(index)
          sss$w[index] <- spd$w[index] + otherFleet$w[irow]

        }
      }

    }
    # remove all other categories that have lengths
    sss <- sss %>% dplyr::filter(!  ((hydraFleets == "other") & (!is.na(bins)))  )

    # now length data combined and other removed. calculate proportion for length bins
    proportions <- sss %>% dplyr::group_by(YEAR,hydraFleets) %>%
      dplyr::filter(hydraFleets != "other") %>%
      dplyr::filter(!is.na(bins)) %>%
      dplyr::mutate(prop = w/sum(w)) %>%
      dplyr::ungroup()

    # Update total biomass by fleets. This adds in landings NA for gear types but no lengths
    totalBio <- sss %>% dplyr::group_by(YEAR,hydraFleets) %>%
      dplyr::summarise(binTot=sum(w),.groups="drop")

    # now deal with the other gear NA. no length bin data
    # add to total landings for most dominant fleet
    for (iy in unique(sss$YEAR)) {
      otherFleet <- sss %>%
        dplyr::filter(YEAR == iy) %>%
        dplyr::filter(hydraFleets == "other")

      if (nrow(otherFleet) < 1){
        next
      }
      for (irow in 1:nrow(otherFleet)){
        # dominantFleet <- dominantFleetByYear %>%
        #   dplyr::filter(YEAR == iy) %>%
        #   dplyr::pull(hydraFleets)

        if (is.na(otherFleet$bins[irow])) {
          # no bin data. Add weight to dominant gears total weight (binTot)
          # character name of fleet
          domFleet <- dominantFleetByYear$hydraFleets[dominantFleetByYear$YEAR==iy]
          index <- (totalBio$YEAR==iy) * (totalBio$hydraFleets==domFleet)
          index[is.na(index)] <- 0
          index <- as.logical(index)
          totalBio$binTot[index] <- totalBio$binTot[index] + otherFleet$w[irow]

        } else {
          stop("should already have dealt with this")
        }
      }

    }

    sss <- sss %>% dplyr::filter(hydraFleets != "other")
    # remove other
    totalBio <- totalBio %>%
      dplyr::filter(hydraFleets != "other")

  # wide format
  spProps <- proportions %>%
    tidyr::pivot_wider(.,id_cols=c(YEAR,hydraFleets),names_from = bins, values_from = prop,names_sort=T)
  finalTable <- totalBio %>%
    dplyr::left_join(.,spProps,by=c("YEAR","hydraFleets")) %>%
    dplyr::mutate(species_itis=itis) %>%
    dplyr::rename(year = YEAR) %>%
    dplyr::mutate(type = 0) %>%
    dplyr::rename(totalBiomass = binTot) %>%
    dplyr::rename(fleet=hydraFleets)

  # find missing sizeclasses
  missingCols <- setdiff(names(hydradata::hydraDataList$binwidth),names(spProps) )
  # create thes missing columns
  finalTable[missingCols] <- as.numeric(NA)
  finalTable["area"] <- "GB"

  finalTable <- finalTable %>%
    dplyr::relocate(fleet,area,year,species_itis,type,totalBiomass, paste0("sizeclass",1:length(bins)))

  print(finalTable)

#  return(list(totalBio=totalBio,sss=sss,spd=spd,dominantFleetByYear=dominantFleetByYear,props=proportions))
  return(finalTable)
}


