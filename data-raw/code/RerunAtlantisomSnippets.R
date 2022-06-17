# rerun weight at age interpolation for fishery data only
# after June 2022 bugfix to atlantisom::calc_avgwtstage2age

# saved files local to Sarah's machine

omlist_ss <- readRDS("~/Documents/0_Data/ms-keyrun/simulated-data/atlantisoutput/NOBA_sacc_38/nordic_runresults_01omlist_ss.rds")

fishObsWtAtAge <- readRDS("~/Documents/0_Data/ms-keyrun/simulated-data/atlantisoutput/NOBA_sacc_38/nordic_runresults_01_censusfishObsWtAtAge.rds")

scenario.name <- "nordic_runresults_01"
fishery.name <- "census"
d.name <- "~/Documents/0_Data/ms-keyrun/simulated-data/atlantisoutput/NOBA_sacc_38"
n_reps <- 1
save <- TRUE

library(atlantisom)

# snippet of atlantisom::om_comps()
# do we want fishery average weight at true age too? why not
# call interpolate weight at age function to get fishObsFullWtAtAge
# WARNING currently aggregates out fleet info, but no fleets in aggregate wtage
if(!is.null(omlist_ss$truecatchage_ss)){
  interp_fishWtAtAge <- list()
  for(i in 1:n_reps){
    interp_fishWtAtAge[[i]] <- calc_avgwtstage2age(wtagecl = fishObsWtAtAge[[i]],
                                                   annages = omlist_ss$truecatchage_ss,
                                                   fgs = omlist_ss$funct.group_ss)
  }
  if(save){
    saveRDS(interp_fishWtAtAge, file.path(d.name, paste0(scenario.name,"_",
                                                         fishery.name, "fishObsFullWtAtAge.rds")))
  }
}else{interp_fishWtAtAge <- NULL}
