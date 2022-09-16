#' Create real diet datasets for mskeyrun
#' 
#' Diet dataset built by Brian Smith on github is accessed, mean wt and proportion 
#' of prey in diet by species with variance is calculated
#' 
#' Modified \code{get_diet} function by Sean Hardison, original here
#' https://github.com/NOAA-EDAB/ECSA/blob/master/R/get_diet.R
#' Creates a dataset of annual and seasonal weighted diet composition with sample sizes.
#' Datasets are for a selected species and set of seasonal survey strata.
#' Need to clearly define what each data column is, render to tidy data.
#' This is a first draft that will be revised with dplyr functions later.
#' n.b., Original file from Brian Smith, August 23 2018 was allwt_nstoms.R, renamed as get_diet.R
#'
#' 
#'@param focalspp mskeyrun dataset, output of \code\link{{create_focal_species}} or vector of ALL CAPS predator scientific names
#'@param survstrat list of NEFSC trawl survey strata
#'@param saveToData Boolean. Export to data folder (Default = T)
#'
#'@return A tibble (Also written to \code{data} folder)
#'dataframe with variables svspp, year, season, meansw, num_tows, variance, cv, prey, totwt, relmsw, ci, relci, nstom
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{year}{year diet data collected}
#'\item{Code}{SPECIES_ITIS code for predator species}
#'\item{Name}{Common name for predator species}
#'\item{season}{season diet data collected}
#'\item{prey}{Common name for prey item}
#'\item{variable}{mean prey item weight in year/species/season (meansw),
#'                variance of meansw (variance),
#'                number of tows in year/species/season/prey (num_tows),
#'                cv of meansw (cv),
#'                confidence interval for mean prey weight in diet (ci),
#'                total prey weight in year/species/season (totwt),
#'                percent of prey item in diet in year/species/season (relmsw),
#'                confidence interval for percent prey in diet (relci),
#'                number of stomachs in year/species/season (nstom)}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable; grams for meansw, variance, ci, totwt,
#'             percent for relmsw, relci, numbers for num_tows, nstom, unitless cv}
#'

library(magrittr)
library(dplyr)

create_real_dietcomp <- function(focalspp, survstrat, saveToData = T){
  
  # object is called `allfh`
  load(url("https://github.com/Laurels1/Condition/raw/master/data/allfh.RData"))
  
  ## Filter out only good stomachs for strata and species
  allsum_raw <- allfh %>%
    dplyr::filter(pdscinam %in% unique(focalspp$SCIENTIFIC_NAME),
                  stratum %in% survstrat,
                  pynam != 'BLOWN', 
                  pynam != 'PRESERVED',
                  pynam != ' ',
                  !is.na(year),
                  purcode == 10)#,
  
  # all below to line 510 identical to get_diet.R
  ## Number of stomachs by length and weight
  
  ## Sum by length
  allsum_len <- allsum_raw %>%
    group_by(year,
             season,
             svspp,
             cruise,
             station,
             pdid,
             pdsex,
             pdlen) %>%
    summarise(totwt = sum(pyamtw)) %>%
    na.omit() %>%
    group_by(year,
             season,
             svspp) %>%
    summarise(nstom    = n(),
              meanstom = mean(totwt, na.rm = TRUE),
              varstom  = var(totwt, na.rm = TRUE),
              meanlen  = mean(pdlen, na.rm = TRUE),
              numlen   = n(),
              minstom  = min(totwt, na.rm = TRUE),
              maxstom  = max(totwt, na.rm = TRUE),
              minlen   = min(pdlen, na.rm = TRUE),
              maxlen   = max(pdlen, na.rm = TRUE))
  
  ## Sum by weight
  allsum_wgt <- allsum_raw %>%
    group_by(year,
             season,
             svspp,
             cruise,
             station,
             pdid,
             pdsex,
             pdwgt,
             pdlen) %>%
    summarise(totwt = sum(pyamtw, na.rm = TRUE)) %>%
    na.omit() %>%
    group_by(year,
             season,
             svspp) %>%
    summarise(meanwgt = mean(pdwgt, na.rm = TRUE),
              numwgt  = n(),
              minwgt  = min(pdwgt, na.rm = TRUE),
              maxwgt  = max(pdwgt, na.rm = TRUE))
  
  #stomstats2
  nstom_df <- allsum_len %>%
    left_join(allsum_wgt, by = c("year", "season", "svspp")) %>%
    # mutate(std.error = sqrt(varstom/nstom)) %>% 
    select(svspp, year, season, nstom)
  
  #----------------------------------Start weighted diet proportion code-------------------------------------#
  
  ## Select and rename the appropriate columns
  allsum_strat <- allsum_raw %>%
    select(cruise6, stratum, station, 
           year,
           season,
           svspp, pdid, 
           pdsex, pdlen, 
           tax = collsci, 
           pyamt = pyamtw, 
           catnum, numlen, 
           tot_catnum_stratum, tot_catwgt_stratum, tot_tows_spp_stratum, 
           stratum_area)
  
  ## Group into seasonal length-class and remove NAs
  num_strat <- allsum_strat %>%
    group_by(cruise6,
             station,
             year,
             season,
             svspp,
             pdid,
             pdsex,
             pdlen) %>%
    summarise(xxxx = n()) %>%
    group_by(cruise6,
             station, 
             year,
             season, 
             svspp, 
             pdsex, 
             pdlen) %>%
    summarise(numlen2 = n()) %>%
    left_join(allsum_strat,  by = c("cruise6", "station", "year",
                                    "season", "svspp", "pdsex", "pdlen")) %>% 
    mutate(numlen_fin = case_when(numlen < numlen2 ~ numlen2,
                                  numlen >= numlen2 ~ numlen,
                                  TRUE ~ NA_integer_)) %>%
    select(c(cruise6, stratum, station,
             year,
             season, 
             svspp, pdid, pdsex, pdlen, tax, pyamt, 
             catnum, tot_catnum_stratum, tot_catwgt_stratum,
             tot_tows_spp_stratum, stratum_area, numlen, numlen_fin))
  
  
  sum_strat <- num_strat %>%
    group_by(cruise6,
             station, 
             year, 
             season,
             svspp, 
             pdsex,
             pdlen, 
             numlen_fin) %>%
    summarise(dummy_var = sum(numlen, na.rm = TRUE)) %>% 
    na.omit() %>%
    group_by(cruise6,
             station, 
             year, 
             season, 
             svspp, 
             pdlen) %>%
    summarise(rnumlen_fin = sum(numlen_fin, na.rm = TRUE)) %>%
    left_join(num_strat, by = c("cruise6", "station", "year", "season", "svspp", "pdlen")) %>% 
    mutate(rnumlen_fin = ifelse(svspp !=13 | svspp !=15, 
                                numlen_fin, 
                                rnumlen_fin)) %>%
    group_by(cruise6, 
             station,
             year,
             season,
             svspp, 
             pdsex, 
             catnum) %>%
    summarise(dummy_var = sum(numlen)) %>% 
    na.omit()
  
  
  sum_catnum <- sum_strat %>%
    group_by(cruise6,
             station, 
             year,
             season,
             svspp) %>%
    summarise(rcatnum = sum(catnum, na.rm = TRUE)) %>%
    na.omit() %>%
    left_join(num_strat,  by = c("cruise6", "station", "year", "season", "svspp")) %>% 
    mutate(rcatnum = ifelse(svspp != 13 | svspp != 15, 
                            catnum, 
                            rcatnum))
  
  sum_catstratum <- sum_catnum %>%
    group_by(cruise6, stratum, 
             year,
             season, 
             svspp, 
             pdsex, 
             tot_tows_spp_stratum,
             tot_catnum_stratum,
             tot_catwgt_stratum) %>%
    summarise(dum_var = sum(rcatnum, na.rm = TRUE)) %>%
    na.omit()
  
  ####################################################################################################################################
  
  max_tot_tows_spp_stratum <- sum_catstratum %>%
    group_by(cruise6, 
             stratum, 
             year,
             season,
             svspp) %>% 
    summarise(tot_tows_spp_stratum = max(tot_tows_spp_stratum, na.rm = TRUE))
  
  sum_rcatstratum <- sum_catstratum %>%
    group_by(cruise6, 
             stratum, 
             year,
             season,
             svspp) %>%
    summarise(rtot_catnum_stratum = sum(tot_catnum_stratum, na.rm = TRUE),
              rtot_catwgt_stratum = sum(tot_catwgt_stratum, na.rm = TRUE)) %>% 
    na.omit() %>%
    left_join(max_tot_tows_spp_stratum, by = c("cruise6", "stratum", "year", "season", "svspp")) %>%
    rename(rtot_tows_spp_stratum = tot_tows_spp_stratum)
  
  final_strat <- sum_rcatstratum %>%
    left_join(sum_catnum, by = c("cruise6", "stratum", "year", "season", "svspp")) %>% 
    mutate(rtot_catnum_stratum = ifelse(svspp != 13 | svspp != 15, 
                                        tot_catnum_stratum, 
                                        rtot_catnum_stratum),
           rtot_catwgt_stratum = ifelse(svspp != 13 | svspp != 15, 
                                        tot_catwgt_stratum, 
                                        rtot_catwgt_stratum),
           rtot_tows_spp_stratum = ifelse(svspp != 13 | svspp != 15, 
                                          tot_tows_spp_stratum,
                                          rtot_tows_spp_stratum))
  
  ####################################################################################################################################
  py_raw <- final_strat %>%
    group_by(cruise6, 
             station, 
             year,
             season, 
             svspp, pdid, pdlen, tax) %>%
    summarise(pysum = sum(pyamt, na.rm = TRUE)) %>% 
    na.omit() 
  
  py_nostom <- py_raw %>%
    ungroup() %>% 
    arrange(cruise6, station,
            year,
            season,
            svspp,
            pdid) %>%
    group_by(cruise6,
             station, 
             year,
             season, 
             svspp, 
             pdid) %>% 
    top_n(-1, wt = pysum) %>%
    group_by(cruise6, 
             station, 
             year, 
             season, 
             svspp) %>%
    summarise(nostom = n())
  
  py_list <- data.frame(tax = unique(py_raw$tax),
                        pycode = paste0('prey', 1:length(unique(py_raw$tax))))
  
  
  py_all <- py_nostom %>%
    left_join(py_raw, by = c("cruise6", "station", "year", "season", "svspp")) %>% 
    left_join(py_list, by = "tax")
  
  
  pd_strat <- final_strat %>%
    group_by(cruise6, 
             station, 
             year,
             season,
             svspp, 
             pdid, 
             pdlen) %>% 
    top_n(-1, wt = stratum_area) 
  
  
  pd_nas <- pd_strat %>%
    group_by(cruise6, 
             station, 
             year,
             season,
             svspp, 
             pdlen) %>%
    summarise(rnumlen_fin = sum(numlen_fin, na.rm = TRUE)) %>%
    na.omit()
  
  pd_strat <- pd_strat %>%
    left_join(pd_nas, by = c("cruise6", "year", "season", "svspp", "station", "pdlen")) %>% 
    select(cruise6, station, stratum, 
           year,
           season,
           svspp, pdid, pdlen,
           rcatnum, rtot_catnum_stratum,
           tot_catwgt_stratum, rtot_catwgt_stratum,
           rtot_tows_spp_stratum, stratum_area, rnumlen_fin)
  
  #Working with fish data.frame----------------------------------------------------------------------------------#
  fish_ply <- py_all %>%
    select(-tax) %>% 
    left_join(pd_strat, by = c("cruise6", "station", "year", "season", "svspp", "pdid", "pdlen"))
  
  
  ####################################################################################################################################
  ## Take the median value for each prey group
  trans_ply <- fish_ply %>%
    group_by(cruise6, stratum, station,
             year, season, 
             svspp, rcatnum,
             rtot_catnum_stratum, 
             rtot_catwgt_stratum,
             rtot_tows_spp_stratum,
             stratum_area, rnumlen_fin, pycode) %>%
    summarize(pysum = median(pysum, na.rm = FALSE))
  
  ## Weighted mean and mean of pysum
  pysum_wm <- trans_ply %>%
    group_by(cruise6, stratum, station, 
             year, season,
             svspp, rcatnum, 
             rtot_catnum_stratum,
             rtot_tows_spp_stratum,
             stratum_area, pycode) %>% 
    summarize(wmean = weighted.mean(pysum, w = rnumlen_fin))# %>%
  
  ## Mean
  pysum_m <- pysum_wm %>%
    group_by(cruise6, stratum, station,
             year, season,
             svspp, rcatnum,
             rtot_catnum_stratum,
             rtot_tows_spp_stratum,
             stratum_area,
             pycode) %>%
    summarize(musw = mean(wmean, na.rm = TRUE),
              musw2 = wmean * rcatnum)
  
  ## by station
  pysum_wm_s <- pysum_m %>%
    group_by(cruise6, stratum, 
             year, season,
             svspp,
             rtot_catnum_stratum,
             rtot_tows_spp_stratum,
             stratum_area, 
             pycode) %>% 
    summarize(tmsw_strat = sum(musw2, na.rm = TRUE))
  
  musw_strat_ply <- pysum_wm_s %>% 
    group_by(cruise6, stratum, 
             year, season,
             svspp,
             rtot_catnum_stratum,
             rtot_tows_spp_stratum,
             stratum_area, 
             pycode) %>% 
    summarize(musw_strat = tmsw_strat/rtot_tows_spp_stratum) %>%
    ungroup() %>%
    mutate(munfish_strat = rtot_catnum_stratum / rtot_tows_spp_stratum)
  
  
  munfish_stratdat_ply <- musw_strat_ply %>%
    ungroup() %>%
    mutate(munfish_strat = rtot_catnum_stratum / rtot_tows_spp_stratum) %>%
    select(cruise6, stratum,
           year, season,
           svspp, munfish_strat)
  
  
  #weight by stratum area
  pymean_strat <- musw_strat_ply %>% 
    group_by(svspp, year, season,
             pycode) %>% 
    summarize(msw_strat = weighted.mean(musw_strat, stratum_area, na.rm = TRUE),
              m_nfish_strat = weighted.mean(munfish_strat, stratum_area, na.rm = TRUE),
              num_stra = n())
  
  
  meansw_s_ply <- pymean_strat %>% 
    group_by(svspp, year, season, pycode) %>%
    summarize(meansw_s = msw_strat/m_nfish_strat)
  
  meansw_ply <- pysum_m %>% 
    group_by(svspp, year, season, pycode) %>%
    summarize(meansw = weighted.mean(musw, rcatnum, na.rm = TRUE),
              num_tows = n())
  
  master_ply <- pysum_m %>% #musw and musw2
    left_join(pysum_wm_s) %>%  #tmsw_strat
    left_join(musw_strat_ply) %>% # musw_strat
    left_join(meansw_ply) %>% # meansw
    left_join(meansw_s_ply) %>% # meansw_s
    left_join(pymean_strat) %>%     # msw_strat
    select(-munfish_strat) %>% 
    mutate(prod = rcatnum^2 * ((musw - meansw))^2,
           prodf = (rcatnum - m_nfish_strat)^2,
           prodd = (musw2 - musw_strat)^2,
           prod_cov = (rcatnum - m_nfish_strat) * (musw2 - musw_strat))# %>% 
  
  mprod_mnumfish2_ply <- master_ply %>%
    group_by(svspp, year, season, pycode) %>%
    summarize(mprod = mean(prod, na.rm = TRUE),
              mnumfish = mean(rcatnum, na.rm = TRUE))
  
  new4_ply <- master_ply %>% 
    group_by(svspp, year, season, pycode) %>% 
    summarize(sprod = sum(prod, na.rm = TRUE),
              mprod = mean(prod, na.rm = TRUE),
              mnumfish = mean(rcatnum, na.rm = TRUE))
  
  new6_ply <- master_ply %>% 
    group_by(svspp, year, season,
             pycode, cruise6, stratum,
             stratum_area, rtot_tows_spp_stratum,
             m_nfish_strat) %>%
    summarize(sprodf = sum(prodf, na.rm = TRUE),
              sprodd = sum(prodd, na.rm = TRUE),
              sprod_cov = sum(prod_cov, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(dfntows_strat = rtot_tows_spp_stratum - 1,
           varprodf = (stratum_area^2) * ((sprodf/dfntows_strat)/rtot_tows_spp_stratum),
           varprodd = (stratum_area^2) * ((sprodd/dfntows_strat)/rtot_tows_spp_stratum),
           varprod_cov = (stratum_area^2) * ((sprod_cov/dfntows_strat)/rtot_tows_spp_stratum),
           varprodf = if_else(rtot_tows_spp_stratum > 1,
                              varprodf,
                              0),
           varprodd = if_else(rtot_tows_spp_stratum > 1,
                              varprodd,
                              0),
           varprod_cov = if_else(rtot_tows_spp_stratum > 1, 
                                 varprod_cov,
                                 0))
  
  sumvarprod_ply <- new6_ply %>% 
    group_by(svspp, year, season, pycode) %>%
    summarize(svarprodf = sum(varprodf, na.rm = TRUE),
              svarprodd = sum(varprodd, na.rm = TRUE),
              svarprod_cov = sum(varprod_cov, na.rm = TRUE),
              stratum_area = sum(stratum_area, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(varf = svarprodf / stratum_area^2,
           vard = svarprodd/ stratum_area^2,
           var_cov = svarprod_cov/ stratum_area^2) %>% 
    select(-stratum_area)
  
  new4_strat_ply <- new4_ply %>% 
    left_join(sumvarprod_ply) 
  
  
  #merge with  select master columns
  six_ply <- new4_strat_ply %>%
    left_join(master_ply) %>% 
    mutate(variance = ifelse(num_tows > 1 & mnumfish != 0 & meansw!=0 & m_nfish_strat !=0 & meansw_s!=0,
                             1/(num_tows * (mnumfish)^2) * (sprod/(num_tows-1)),
                             0),
           cv = ifelse(num_tows > 1 & mnumfish != 0 & meansw != 0 & m_nfish_strat != 0 & meansw_s != 0,
                       ((variance)^0.5)/meansw,
                       0),
           var_s = ifelse(num_tows > 1 & mnumfish != 0 & meansw != 0 & m_nfish_strat != 0 & meansw_s != 0,
                          (meansw_s^2) * ((varf/(m_nfish_strat^2)) + (vard/(msw_strat^2)) - (2 * var_cov/m_nfish_strat/msw_strat)),
                          0),
           cv_s = ifelse(num_tows > 1 & mnumfish != 0 & meansw != 0 & m_nfish_strat != 0 & meansw_s != 0,
                         ((var_s)^0.5)/meansw_s,0)) %>% 
    select(pycode, svspp, year, season, meansw,
           meansw_s, num_tows, m_nfish_strat, num_stra, 
           variance, cv, var_s, cv_s, pycode) %>% 
    distinct(.keep_all = TRUE)
  
  final_ply <- six_ply %>% 
    mutate(pycode = as.factor(pycode)) %>% 
    left_join(py_list) %>% 
    left_join(nstom_df) %>% 
    group_by(svspp, year, season) %>% 
    mutate(totwt = sum(meansw, na.rm = TRUE),
           totwt_s = sum(meansw_s, na.rm = TRUE),
           relmsw = 100*(meansw/totwt),
           relmsw_s = 100*(meansw_s/totwt_s),
           ci = sqrt(variance/num_tows)*2,
           relci = (ci/totwt)*100,
           ci_s = sqrt(var_s/num_tows)*2,
           relci_s = (ci_s/totwt_s)*100) %>% 
    select(svspp, year, season, meansw, num_tows,
           variance, cv, prey = tax, totwt, 
           relmsw, ci, relci, nstom)
  
  
  # SKG added below to format similarly to other mskeyrun datasets
  # long dataset with variables:
  # mean prey item weight in year/species/season (meansw),
  # variance of meansw (variance),
  # cv of meansw (cv),
  # confidence interval for mean prey weight in diet (ci),
  # total prey weight in year/species/season (totwt),
  # percent of prey item in diet in year/species/season (relmsw),
  # confidence interval for percent prey in diet (relci),
  # number of tows in year/species/season (num_tows),
  # number of stomachs in year/species/season (nstom)
  
  survdiet <- final_ply %>%
    dplyr::left_join(focalspp, by = c("svspp" = "SVSPP")) %>%
    dplyr::mutate(ModSim = "Actual",
                  Code = SPECIES_ITIS,
                  Name = modelName) %>%
    dplyr::ungroup() %>%
    dplyr::select(ModSim, year, Code, Name, season, prey,
                  meansw, num_tows, variance, cv, totwt, relmsw,
                  ci, relci, nstom) %>%
    tidyr::pivot_longer(cols = c("meansw", "num_tows", "variance", "cv", "totwt", "relmsw",
                                 "ci", "relci", "nstom"), 
                        names_to = "variable",
                        values_to = "value") %>%
    dplyr::mutate(units = case_when(variable %in% c("meansw", "variance","totwt","ci") ~ "grams", 
                                    variable %in% c("relmsw", "relci") ~ "percent",
                                    variable %in% c("num_tows","nstom") ~ "number",
                                    variable %in% c("cv") ~ "unitless",
                                    TRUE ~ as.character("NA")))
  
  # reclassify variables that apply to pred not each prey
  allprey <- survdiet %>%
     dplyr::filter(variable %in% c("totwt", "nstom")) %>%
     dplyr::mutate(prey = "ALL") %>%
     dplyr::distinct()
  
  # remove duplicates and bind in reclassified pred variables
  eachprey <- survdiet %>%
    filter(variable != "totwt", 
           variable != "nstom") 
  
 
  survdietfin <- bind_rows(eachprey, allprey)
  
  surveyDietcomp <- survdietfin
  
  if (saveToData) {
    
    usethis::use_data(surveyDietcomp, overwrite = TRUE)
  }
  
  return(surveyDietcomp)
  
}