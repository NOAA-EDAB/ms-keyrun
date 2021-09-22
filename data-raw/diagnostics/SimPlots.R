#' plot code from DiagnosticPlotExamples.Rmd sourced
#' and adjusted here for on the fly checks of mskeyrun data
#' could put in html but currently show up in rstudio plots pane


for(s in unique(mskeyrun::simSurveyAgecomp$survey)){
  cat("  \n##### ",  s,"  \n")
  acompsub <- filter(mskeyrun::simSurveyAgecomp, survey==s, year %in% c(70:80)) %>%
    rename(time = year,
           agecl = age,
           atoutput = value,
           species = Name) %>%
    group_by(species) %>%
    #left_join(., trueNage) %>%
    group_map(~ Natageplot(.x), keep = TRUE) # plots only sampled age comp
    #group_map(~ Natageplot(.x, effN = 100000, truedat = 1), keep = TRUE) # plots merged true age comp
  
  for(i in 1:length(acompsub)) {
    print(acompsub[[i]])
  }
  cat("  \n")
}

for(s in unique(mskeyrun::simSurveyLencomp$survey)){
  cat("  \n##### ",  s,"  \n")
  lcompsub <- filter(mskeyrun::simSurveyLencomp, year %in% c(40:50)) %>%
    rename(time = year,
           upper.bins = lenbin, #plot function uses upper.bins but dataset uses lower
           atoutput = value,
           species = Name) %>%
    group_by(species) %>%
    group_map(~ plotlen(.x), keep = TRUE)
  
  for(i in 1:length(lcompsub)) {
    print(lcompsub[[i]])
  }
  cat("  \n")
}

for(s in unique(mskeyrun::simFisheryLencomp$fishery)){
  cat("  \n##### ",  s,"  \n")
  lcompsub <- filter(mskeyrun::simFisheryLencomp, year %in% c(50:60)) %>%
    rename(time = year,
           upper.bins = lenbin, #plot function uses upper.bins but dataset uses lower
           atoutput = value,
           species = Name) %>%
    group_by(species) %>%
    group_map(~ plotlen(.x), keep = TRUE)
  
  for(i in 1:length(lcompsub)) {
    print(lcompsub[[i]])
  }
  cat("  \n")
}
