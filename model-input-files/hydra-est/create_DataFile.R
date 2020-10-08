# we create data files for use in Hydra (.dat and .pin)
# all of the values are stored or read in to this script then written out to a text file.
# the goal is to avoid using data files without fully understanding their content.
# We hope to achieve this by including plots of input variables and creating small output readMe files to summarize input file
#
# Entries in .dat file need to be in a specific order, hence the long script
#
# Parts of data that user may want to control
# scenarioFlag = "historical" or "assessment". 
# temperatureFlag = "true" or "mean".  Uses either the observed data or the mean of the observed data
# scenarioType = "Fixed","Ramp".   Fixed = Fixed eploitation rate. Ramp = ramp down. This is only used if assessment is chosen as scenarioFlag.
# ExploitationRateFlag = "5,10,15,20,25,30,35,40. Levels of maximum exploitation in either ramp or fixed scenarioType. Used only if scenarioFlag is assessment
# assessmentSpeciesFlag =  "high", "low", "none". "none" = species levels not part of assessment. "low" included at same rate as complex. "high" = extra elasmobrach protection
# outputFileName = "any name you ant to give"
#
# Effort Fishing = implied by use of assessment and exploitation rate, or historical


#setwd("D:/MyWork/Hydra/Beet-Hydra/")

create_DataFile <- function(scenarioFlag="historical",temperatureFlag="true",scenarioType="fixed",exploitationRateFlag=30,assessmentSpeciesFlag="none",outputFileName="hydra_sim_guild_1_3_3"){

  # complete error checks to make sure all data conforms
  options <- list()
  options$pathToDataFiles <- "dataInputsHydra"
  
  
  options$fillLength <- 2000 # length of line to write to. if not long enough data wraps to next line
  options$scenarioFlag <- scenarioFlag
  options$exploitationRate <- exploitationRateFlag
  options$assessmentWithSpecies <- assessmentSpeciesFlag
  options$temperatureFlag <- temperatureFlag
  
  # read in data options
  d <- get_DatData(options)
  p <- get_PinData(options)

  if (tolower(temperatureFlag) == "mean") { # take the mean of the temp time series
    d$observed_temperature <- rep(mean(d$observed_temperature),d$Nyrs)
  } else {
    # do nothing since observesd is read in 
  }
  
    
  if (tolower(scenarioFlag) == "historical") {
    # we assume a historical run. True Temp, True Effort, No asessment, Rec error, no survey error
    options$assessmentOn <- 0
    options$assessmentWithSpeciesOn <- 0 # this is also ignored
    # if assessmentOn = 0. exploitationoption are ignored but still need to be read in .
    d$exploitation <- d$exploitationOptions[,1] # never used when assessment is off but needs a placeholder
    
  } else if  (tolower(scenarioFlag) == "assessment") {
    options$assessmentOn <- 1
    maxRates <- d$exploitationOptions[d$Nthresholds,]*100  # picks out the last row which holds the max exploitation rate for each scenario
    d$exploitation <- d$exploitationOptions[,(maxRates == exploitationRateFlag)] # grabs the whole profile
    print(d$exploitation)
    maxRampExRate <- max(d$exploitation)
    
    if (tolower(scenarioType) == "fixed") {
      # all exploitations are the same
      d$exploitation <- rep(exploitationRateFlag/100,d$Nthresholds)
    } else {
      # we have a ramp down scenario and the values in d$exploitation reflect this
    }
    
    if ((assessmentSpeciesFlag == "none") | (assessmentSpeciesFlag == "low")) {
      d$thresholdSpecies <- d$thresholdSpecies*0
    }
    if (assessmentSpeciesFlag == "none") {
      options$assessmentWithSpeciesOn <- 0
    } else {
      options$assessmentWithSpeciesOn <- 1
    }
    

    # effort needs to change to represent exploitation rate equal to max(rampdown rate)
    for (ifleet in 1:d$Nfleets) {
      fE <- as.numeric(p$fishery_q[,ifleet]) # pick out fishery q's
      ind <- as.numeric(p$fishery_q[,ifleet]) > 1e-29 # find all > 1e-29
      d$observed_effort[ifleet,] <- rep(maxRampExRate/(sum(fE[ind])/sum(ind)),d$Nyrs) # Effort = ex/mean(q)
    }

    
  } else if (tolower(scenarioFlag) == "custom") {
    # code this part if needed
  }
  
  # some error checks
  if ((tolower(scenarioFlag) == "historical") & (d$Nyrs !=53)) {
    stop(paste("Can not have a historical run > ",d$Nyrs,"years. Not enough data!"))
  }
  
  # need to use inputs to create output filename 
  
  
  options$outputDatFileName <- paste0(outputFileName,".dat")
  options$outputPinFileName <- paste0(outputFileName,".pin")
  
    
  # write out dat file  
  write_DatFile(d,options)
  write_PinFile(p,d,options)
  
  # plot functional forms of inputs and summarize important parameters
  
  
  
  #return(fE)
}

## subfunctions get_pinData, get_DatData, write_DatFile,write_PinFile


write_DatFile <- function(d,options) {
  outputFileName <- options$outputDatFileName
  # write explanation of how this file was formed
  cat("# This file was created using create_DataFile.R and used all inputs from csv files found in folder:
      #createDataFiles_testing/dataInputsHydra",file=outputFileName,fill=TRUE)
  
 
  # write all inputs to file with comment headers
  cat("# init_int debug",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$debugState),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_int Nyrs",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Nyrs),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_int Nspecies",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Nspecies),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_int Nsizebins",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Nsizebins),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_int Nareas",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Nareas),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_int Nfleets",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Nfleets),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_number wtconv",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$wtconv),file=outputFileName,fill=TRUE,append=TRUE)
  # speciesList
  cat("#",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# List of Species in Model",file=outputFileName,fill=TRUE,append=TRUE)
  for (sp in d$speciesList) {
    cat(c("#",sp),file=outputFileName,fill=TRUE,append=TRUE)
  }
  cat("#",file=outputFileName,fill=TRUE,append=TRUE)
  
  # length bins
  cat("# init_matrix binwidth(1,Nspecies,1,Nsizebins)",file=outputFileName,fill=TRUE,append=TRUE)
  #write.table(d$binwidth, file=outputFileName, row.names=FALSE, col.names=FALSE,append=TRUE,sep="\t")
  for(sp in 1:d$Nspecies) {
    cat(c(" ",as.matrix(d$binwidth[sp,])), file=outputFileName, fill=TRUE,append=TRUE,sep="\t")
  }
  
  # length- weight relationship. w = aL^b
  cat("# init_vector lenwt_a(1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$lenwt_a),file=outputFileName,fill=TRUE,append=TRUE)
  cat("# init_vector lenwt_b(1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$lenwt_b),file=outputFileName,fill=TRUE,append=TRUE)
  # covariate information, number of covariates
  cat("# init_int Nrecruitment_cov",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Nrecruitment_cov),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int Nmaturity_cov ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Nmaturity_cov),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int Ngrowth_cov",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Ngrowth_cov),file=outputFileName,fill=TRUE,append=TRUE)
  
  # covariate time series  - recruitment, maturity, growth
  cat("#  init_matrix recruitment_cov(1,Nrecruitment_cov,1,Nyrs)",file=outputFileName,fill=TRUE,append=TRUE)
  for (icov in 1:d$Nrecruitment_cov){
    cat(c(" ",d$recruitment_cov[icov,]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }
  cat("#  init_matrix maturity_cov(1,Nmaturity_cov,1,Nyrs)",file=outputFileName,fill=TRUE,append=TRUE)
  for (icov in 1:d$Nmaturity_cov){
    cat(c(" ",d$maturity_cov[icov,]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }

  cat("#  init_matrix growth_cov(1,Ngrowth_cov,1,Nyrs)",file=outputFileName,fill=TRUE,append=TRUE)
  for (icov in 1:d$Ngrowth_cov){
    cat(c(" ",d$growth_cov[icov,]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }

  # observed (survey) biomass
  cat("#   init_3darray obs_survey_biomass(1,Nareas,1,Nspecies,1,Nyrs) ",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#   THESE ARE FROM ATLANTIS AND SHOULD NOT BE USED IN FITTING: REPLACE WITH SURVEY DATA",file=outputFileName,fill=TRUE,append=TRUE)
  for (idata in 1:d$Nspecies){
    cat(c(" ",d$observed_biomass[idata,]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }
  # observed catch
  cat("#   init_3darray obs_catch_biomass(1,Nareas,1,Nspecies,1,Nyrs) ",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#   THESE ARE FROM ASSESSMENTS see Catches.xls placeholder for real catch data",file=outputFileName,fill=TRUE,append=TRUE)
  for (idata in 1:d$Nspecies){
    cat(c(" ",d$observed_catch[idata,]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }
  # observed effort
  cat("#  init_3darray obs_effort(1,Nareas,1,Nfleets,1,Nyrs)  ",file=outputFileName,fill=TRUE,append=TRUE)
  if (options$scenario =="assessment") {
    cat("#  effort based on q with Exploitation Rate = ",options$exploitationRate/100,". Manufactured effort for simulation runs",file=outputFileName,fill=TRUE,append=TRUE)
  }else{
    cat("#  Observed effort. No assessment",file=outputFileName,fill=TRUE,append=TRUE)
  }  
  for (idata in 1:d$Nfleets){
    cat(c(" ",d$observed_effort[idata,]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }
  
  # intake stomach content
  cat("#  init_4darray area1_stomwt(1,Nareas,1,Nspecies,1,Nyrs,Nsizebins)   ",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  read in mean stomach content weight time series from .dat file for intake calculation   ",file=outputFileName,fill=TRUE,append=TRUE)

  for (sp in 1:d$Nspecies) {
    cat(c("# ",d$speciesList[sp]),file=outputFileName,fill=TRUE,append=TRUE)
    for (iy in 1:d$Nyrs) {
      cat(c(" ",d$intake_stomach[sp,]),file=outputFileName,fill=TRUE,append=TRUE)
    }
    
  }

  # observed temperature
  cat("#   init_matrix obs_temp(1,Nareas,1,Nyrs)      ",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  Either observed temperature  or manufactured temperature for simulation runs",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#1977 to 1997 Georges Bank bottom temp from 2011 ESR (1964-1976 set to 8.0) and 1998 to 2010 Georges Bank bottom temp from 2011 ESR",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$observed_temperature),file=outputFileName,fill=options$fillLength,append=TRUE)

  # estimation phases
  cat("#  init_int yr1Nphase            //year 1 N at size estimation phase",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$yr1Nphase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int recphase				//recruitment parameter estimation phas",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$recphase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int avg_rec_phase		//average recruitment estimation phase (could make species specific, currently global)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$avg_rec_phase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int avg_F_phase			//average fishing mort estimation phase (could make species specific, currently global)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$avg_F_phase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int dev_rec_phase		//recruitment deviation estimation phase (could make species specific, currently global)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$dev_rec_phase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int dev_F_phase			//fishing mort deviation estimation phase (could make species specific, currently global)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$dev_F_phase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int fqphase              //fishery q estimation phase",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$fqphase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int sqphase              //survey q estimation phase ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$sqphase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#   init_int ssig_phase           //survey sigma (obs error) phase",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$ssig_phase),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_int csig_phase           //catch sigma (obs error) phase",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$csig_phase),file=outputFileName,fill=TRUE,append=TRUE)
  
  # stock recruitment parameters
  cat("#  init_matrix recGamma_alpha(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$eggRicker_alpha),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recGamma_shape(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model shape parameter",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$eggRicker_shape),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recGamma_beta(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model beta",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$eggRicker_beta),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recDS_alpha(1,Nareas,1,Nspecies)		//SSB Deriso-Schnute model alpha",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$DS_alpha),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recDS_shape(1,Nareas,1,Nspecies)		//SSB Deriso-Schnute model shape parameter",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$DS_shape),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recDS_beta(1,Nareas,1,Nspecies)			//SSB Deriso-Schnute model beta",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$DS_beta),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recGamSSB_alpha(1,Nareas,1,Nspecies)		//SSB gamma alpha",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$gamma_alpha),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recGamSSB_shape(1,Nareas,1,Nspecies)		//SSB gamma shape parameter",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$gamma_shape),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recGamSSB_beta(1,Nareas,1,Nspecies)			//SSB gamma model beta",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$gamma_beta),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recRicker_alpha(1,Nareas,1,Nspecies)		//SSB Ricker model alpha",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$ricker_alpha),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recRicker_shape(1,Nareas,1,Nspecies)		//SSB Ricker model shape parameter=1.0, not used",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$ricker_shape),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recRicker_beta(1,Nareas,1,Nspecies)			//SSB Ricker model beta",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$ricker_beta),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recBH_alpha(1,Nareas,1,Nspecies)		//SSB Beverton Holt model alpha",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$BH_alpha),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recBH_shape(1,Nareas,1,Nspecies)		//SSB Beverton Holt model shape parameter=1.0, not used",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$BH_shape),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recBH_beta(1,Nareas,1,Nspecies)			//SSB Beverton Holt model beta",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$BH_beta),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recShepherd_alpha //SSB S-R Shepherd 3 param",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Shepherd_alpha),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recShepherd_shape //SSB S-R Shepherd 3 param",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Shepherd_shape),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recShepherd_beta //SSB S-R Shepherd 3 param",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Shepherd_beta),file=outputFileName,fill=TRUE,append=TRUE)
  
  # recruitment type 1,2,3,4,5,6
  cat("#  init_ivector rectype(1,Nspecies)  //switch for alternate recruitment functions 1=gamma/Ricker, 2=Deriso-Schnute, 9=avg+devs
# 3=SSB gamma, 4=SSB Ricker, 5=SSB Beverton Holt added April 2014,6=Shepherd (added Beet Mar 2017)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$rec_type),file=outputFileName,fill=TRUE,append=TRUE)
  
  # recruitment stochasticity
  cat("#   init_ivector stochrec(1,Nspecies)  //switch for stochastic recruitment. 1 = add error, 0= no error",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$rec_stochastic),file=outputFileName,fill=TRUE,append=TRUE)
  
  # sex ratio
  cat("#  init_matrix sexratio(1,Nareas,1,Nspecies)  // this is proportion females",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$sexRatio),file=outputFileName,fill=TRUE,append=TRUE)
  
  # recruitment effects
  cat(" #  init_matrix recruitment_covwt(1,Nspecies,1,Nrecruitment_cov)	//recruitment covariate weighting factor",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(c(" ",d$recruit_covEffects[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }
  
  # fecundity
  cat("#//fecundity parameters from .dat file and calculate fecundity at length",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix fecund_d(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$fecundity_d),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix fecund_h(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$fecundity_h),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_3darray fecund_theta(1,Nareas,1,Nspecies,1,Nsizebins))",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(c(" ",d$fecundity_Theta[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }
  
  # maturity
  cat("#  init_matrix maturity_nu(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$maturity_nu),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix maturity_omega(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$maturity_omega),file=outputFileName,fill=TRUE,append=TRUE)
  
  # maturity covariate effects
  cat("#  init_matrix maturity_covwt(1,Nspecies,1,Nmaturity_cov) //maturity covariate weighting factor",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(c(" ",d$maturity_covEffects[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }
  
  # growth
  cat("#//growth parameters from .dat file and calculate simple (no cov) prob of growing through length interval",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix growth_psi(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$growth_psi),file=outputFileName,fill=options$fillLength,append=TRUE)
  cat("#  init_matrix growth_kappa(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$growth_kappa),file=outputFileName,fill=options$fillLength,append=TRUE)
  
  # growth covariate effects
  cat("#  init_matrix growth_covwt(1,Nspecies,1,Ngrowth_cov)// growth covariate weighting factor",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(c(" ",d$growth_covEffects[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }
  
  cat("#  init_matrix vonB_Linf(1,Nareas,1,Nspecies)    //alternate parameterization, vonB growth",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$growth_Linf),file=outputFileName,fill=options$fillLength,append=TRUE)
  cat("#  init_matrix vonB_k(1,Nareas,1,Nspecies)       //alternate parameterization, vonB growth",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$growth_k),file=outputFileName,fill=options$fillLength,append=TRUE)
  cat("#  init_vector growthtype                           //switch for alternate growth types,",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#1 power, 2 power/covariates, 3 vonB, 4 vonB covariates",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$growth_type),file=outputFileName,fill=options$fillLength,append=TRUE)
  
  # phimax
  cat("#  init_number phimax",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$phimax),file=outputFileName,fill=TRUE,append=TRUE)
  
  # intake
  cat("#  init_matrix intake_alpha(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$intake_alpha),file=outputFileName,fill=options$fillLength,append=TRUE)
  cat("#  init_matrix intake_beta(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$intake_beta),file=outputFileName,fill=options$fillLength,append=TRUE)
  
  # M1 - natural mortality (not explained by model)
  cat(" # M1 - natural mortality (not explained by model)",file=outputFileName,fill=TRUE,append=TRUE)  
  cat("#  init_3darray M1(1,Nareas,1,Nspecies,1,Nsizebins)",file=outputFileName,fill=TRUE,append=TRUE)

  for (isp in 1:d$Nspecies) {
    cat(c(" ",d$M1[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }
  
  # foodweb
  cat("#  init_3darray isprey(1,Nareas,1,Nspecies,1,Nspecies)     //preds in columns, prey in rows",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(c(" ",d$foodweb[isp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }
  
  # M2 size preference function
  cat("#  init_matrix preferred_wtratio(1,Nareas,1,Nspecies)     //pred sizebins",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$M2sizePref_mu),file=outputFileName,fill=options$fillLength,append=TRUE)
  cat("#  init_vector sd_sizepref(1,Nspecies)              //pred sizebins",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$M2sizePref_sigma),file=outputFileName,fill=options$fillLength,append=TRUE)
  
  
  # fishery selectivity
  cat("#  //fishery selectivity pars from dat file, for now not area specific",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix fishsel_c(1,Nspecies,1,Nfleets)  //fishery selectivity c par",file=outputFileName,fill=TRUE,append=TRUE)
  cat("  #benthic trawl and pelagic trawl and longline",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(c(" ",d$fisherySelectivity_c[isp,]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }
  
  cat("#  init_matrix fishsel_d(1,Nspecies,1,Nfleets)  //fishery selectivity d par",file=outputFileName,fill=TRUE,append=TRUE)
  cat("  #benthic trawl and pelagic trawl and longline",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(c(" ",d$fisherySelectivity_d[isp,]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }
  
  
  # Following content added after ICES publication by Gaichas et al. 2014
  # Made by Andy Beet from Dec 2016 onward

  # equilibrium Biomass  
  cat("# Following content added after ICES publication by Gaichas et al. 2014",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# Made by Andy Beet from Dec 2016 onward",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# Equilibrium Biomass. B0(1,Nspecies). Tthese values are obtained by running hydra_sim without any error and zero fishing effort",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$B0),file=outputFileName,fill=options$fillLength,append=TRUE)
  
  # guild number + membership
  cat("#number of Guilds numGuilds.(Piscivores(1), Planktivores(2),Benthivores(3))",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$numGuilds),file=outputFileName,fill=options$fillLength,append=TRUE)

  cat("#Guild Membership guildMembership(1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$guildMembership),file=outputFileName,fill=options$fillLength,append=TRUE)
  
  # assessment thresholds and expploiation
  cat("# AssessmentPeriod. Time period (yrs) to assess guild biomass level",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$assessmentPeriod),file=outputFileName,fill=options$fillLength,append=TRUE) 
  cat("# Nthresholds. number of thresholds used for change in exploitation/fishing",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$Nthresholds),file=outputFileName,fill=options$fillLength,append=TRUE) 
  
  cat("# threshold_percent(1,Nthresholds) threshold %ages (of biomass) when action is taken",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# note that must appear in ascending order",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$thresholds),file=outputFileName,fill=options$fillLength,append=TRUE) 
  cat("# exploitation_levels(1,Nthresholds). these must pair with the threshold_percent values",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$exploitation),file=outputFileName,fill=options$fillLength,append=TRUE) 

  # species specific addition to threshold
  cat("# threshold_species(1,Nspecies). Species level detection threshold",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$thresholdSpecies),file=outputFileName,fill=options$fillLength,append=TRUE) 

  # assessment switches
  cat("# int AssessmentOn. Assessment On or Off",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",options$assessmentOn),file=outputFileName,fill=options$fillLength,append=TRUE) 
  cat("# int speciesDetection. include species (in addition to guild) in assessment on or off",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",options$assessmentWithSpeciesOn),file=outputFileName,fill=options$fillLength,append=TRUE) 

    # large fish index cut off for large fish (cm)
  cat("# int LFI_size. (cm). Threshold to determin a large fish. used in LFI metric",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$LFI_size),file=outputFileName,fill=TRUE,append=TRUE) 
  cat("# init_number scaleInitialN.  used to scale initial yr1N abundances found in .pin file",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$scaleInitialN),file=outputFileName,fill=TRUE,append=TRUE) 
  cat("# other food term",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$otherFood),file=outputFileName,fill=TRUE,append=TRUE) 
  
  # scaling effort due to NEUS shel effort and not GB effort.
  cat("#init_matrix effortScaled(1,Nareas,1,Nspecies)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$scaledEffort),file=outputFileName,fill=TRUE,append=TRUE) 
  
  # discard coeffiecients
  cat("# init_4darray discard_Coef(1,Nareas,1,Nspecies,1,Nfleets,1,Nsizebins)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# proportion of each species that is discarded for each fleet(Bottom, Pelagic, Fixed)",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(paste0("# ",d$speciesList[isp]," fleet x sizeclass"),file=outputFileName,fill=TRUE,append=TRUE)
    for (ifleet in 1:d$Nfleets) {
      rowNum <- ((isp-1)*d$Nfleets) + ifleet
      cat(c(" ",d$discardCoef[rowNum,]),file=outputFileName,fill=TRUE,append=TRUE)
    }
  }
  # survival coefficients
  cat("# init_4darray discardSurvival_Coef(1,Nareas,1,1,Nspecies,1,Nfleets,1,Nsizebins)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# proportion of discards that survive being thrown back",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(paste0("# ",d$speciesList[isp]," fleet x sizeclass"),file=outputFileName,fill=TRUE,append=TRUE)
    for (ifleet in 1:d$Nfleets) {
      rowNum <- ((isp-1)*d$Nfleets) + ifleet
      cat(c(" ",d$discardSurvival[rowNum,]),file=outputFileName,fill=TRUE,append=TRUE)
    }
  }
  
  # predator or prey - binary - for indices
  cat("# predOrPrey(1,Nspecies). binary vector indicating predators. inverse = prey",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$predOrPrey),file=outputFileName,fill=TRUE,append=TRUE) 
  
  # bandidth for smoother for catch sd.
  cat("# bandwidth_metric. (in yrs) for variance estimate of catch - moving window",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$bandwidthMetric),file=outputFileName,fill=TRUE,append=TRUE) 
  
  # end of file
  cat("# eof",file=outputFileName,fill=TRUE,append=TRUE)
  cat("54321",file=outputFileName,fill=TRUE,append=TRUE)
  
  
}


get_DatData <- function(options){
  # We stipulate all of the input data needed to write to the .dat file
  # Eventually it will be better to read all of these in from text files or a GUI. For now this will suffice
  d <- list() # set up list for data storage
  
  # path to data
  path <- paste0(getwd(),"/",options$pathToDataFiles)
  
  # list of species and guilds (Functional Groups)
  speciesList <- read.csv(paste0(path,"/speciesList.csv"),header=TRUE)
  d$speciesList <- as.character(speciesList$species)
  d$guild <- as.character(speciesList$guild)
  d$numGuilds <- length(unique(d$guild))
  d$guildMembership <- speciesList$guildmember
  d$predOrPrey <- speciesList$predOrPrey

  
  singletons <- read.csv(paste0(path,"/singletons.csv"),header=FALSE,row.names = 1)
  d$debugState <- singletons["debug",]
  d$Nyrs <- singletons["Nyrs",]
  d$Nspecies <- singletons["Nspecies",]
  d$Nsizebins <- singletons["Nsizebins",]
  d$Nareas <- singletons["Nareas",]
  d$Nfleets <- singletons["Nfleets",]
  d$wtconv <- singletons["wtconv",]
  d$yr1Nphase <- singletons["yr1Nphase",]
  d$recphase <- singletons["recphase",]
  d$avg_rec_phase <- singletons["avg_rec_phase",]
  d$avg_F_phase <- singletons["avg_F_phase",]
  d$dev_rec_phase <- singletons["dev_rec_phase",]
  d$dev_F_phase <- singletons["dev_F_phase",]
  d$fqphase <- singletons["fqphase",]
  d$sqphase <- singletons["sqphase",]
  d$ssig_phase <- singletons["ssig_phase",]
  d$csig_phase <- singletons["csig_phase",]
  d$phimax <- singletons["phimax",]
  d$scaleInitialN <- singletons["scaleInitialN",]
  d$otherFood <- singletons["otherFood",] 
  d$LFI_size <- singletons["LFI_size",]
  d$bandwidthMetric <- singletons["bandwidth_metric",]
  d$assessmentPeriod <- unlist(singletons["assessmentPeriod",])
  # sizebin lengths
  binwidth <- read.csv(paste0(path,"/length_sizebins.csv"),header=TRUE)
  d$binwidth <- binwidth[1:d$Nspecies,1:d$Nsizebins]

  # length to weight coefficients/parameters
  lenwt <- read.csv(paste0(path,"/lengthweight_species.csv"),header=TRUE)
  d$lenwt_a <- lenwt$a
  d$lenwt_b <- lenwt$b

  # covariate information relating to recruitment, growth and maturity
  recruitmentCovs <- read.csv(paste0(path,"/recruitment_covariates.csv"),header=TRUE)
  maturityCovs <- read.csv(paste0(path,"/maturity_covariates.csv"),header=TRUE)
  growthCovs <- read.csv(paste0(path,"/growth_covariates.csv"),header=TRUE)
  
  d$recruitment_cov <- t(recruitmentCovs)
  d$maturity_cov <- t(maturityCovs)
  d$growth_cov <- t(growthCovs)
  # number of covariates
  d$Nrecruitment_cov <- dim(d$recruitment_cov)[1]
  d$Nmaturity_cov <- dim(d$maturity_cov)[1]
  d$Ngrowth_cov <- dim(d$growth_cov)[1]
  
  # observed survey biomass
  obsBio <- read.csv(paste0(path,"/observation_biomass.csv"),header=TRUE)
  d$observed_biomass <- t(obsBio[,2:(d$Nspecies+1)])

  # observed survey biomass
  obsCatch <- read.csv(paste0(path,"/observation_catch.csv"),header=TRUE)
  d$observed_catch <- t(obsCatch[,2:(d$Nspecies+1)]) 
  
  # observed effort by fleet
  obsEffort <- read.csv(paste0(path,"/observation_effort.csv"),header=TRUE)
  d$observed_effort <- t(obsEffort[,2:(d$Nfleets+1)]) 
  
  # observed temperature
  obsTemp <- read.csv(paste0(path,"/observation_temperature.csv"),header=TRUE)
  d$observed_temperature <- t(obsTemp[,2]) 
  
  # stomach weight
  stomachContent <- read.csv(paste0(path,"/intake_stomachContent.csv"),header=TRUE)
  d$intake_stomach <- as.matrix(stomachContent[,2:(d$Nsizebins+1)])
  
  # recruitment parameters
  stockRecruit <- read.csv(paste0(path,"/recruitment_species.csv"),header=TRUE,row.names=1)

  d$eggRicker_alpha <- unlist(stockRecruit["eggRicker_alpha",])
  d$eggRicker_shape <- unlist(stockRecruit["eggRicker_shape",])
  d$eggRicker_beta <- unlist(stockRecruit["eggRicker_beta",])
  d$DS_alpha <- unlist(stockRecruit["DS_alpha",])
  d$DS_shape <- unlist(stockRecruit["DS_shape",])
  d$DS_beta <- unlist(stockRecruit["DS_beta",])
  d$gamma_alpha <- unlist(stockRecruit["gamma_alpha",])
  d$gamma_shape <- unlist(stockRecruit["gamma_shape",])
  d$gamma_beta <- unlist(stockRecruit["gamma_beta",])
  d$ricker_alpha <- unlist(stockRecruit["ricker_alpha",])
  d$ricker_shape <- unlist(stockRecruit["ricker_shape",])
  d$ricker_beta <- unlist(stockRecruit["ricker_beta",])
  d$BH_alpha <- unlist(stockRecruit["BH_alpha",])
  d$BH_shape <- unlist(stockRecruit["BH_shape",])
  d$BH_beta <- unlist(stockRecruit["BH_beta",])
  d$Shepherd_alpha <- unlist(stockRecruit["shepherd_alpha",])
  d$Shepherd_shape <- unlist(stockRecruit["shepherd_shape",])
  d$Shepherd_beta <- unlist(stockRecruit["shepherd_beta",])
  d$recSigma <- unlist(stockRecruit["sigma",])
  d$rec_type <- unlist(stockRecruit["type",])
  d$rec_stochastic <- unlist(stockRecruit["stochastic",])

  
  # sex ratio
  sexRatio <- read.csv(paste0(path,"/sexratio.csv"),header=TRUE,row.names=1)
  d$sexRatio <- unlist(sexRatio)
  
  # recruitment covariate effects. # columns = d$Nrecruitment_cov
  rec_covEffects <- read.csv(paste0(path,"/recruitment_covariateEffects.csv"),header=TRUE)
  d$recruit_covEffects <- as.matrix(rec_covEffects)
  
  # fecundity
  fecundity_d_h <- read.csv(paste0(path,"/fecundity_species.csv"),header=TRUE,row.names=1)
  d$fecundity_d <- unlist(fecundity_d_h["d",])
  d$fecundity_h <- unlist(fecundity_d_h["h",])
  
  fecundity_Theta <- read.csv(paste0(path,"/fecundity_Theta.csv"),header=TRUE,row.names=1)
  d$fecundity_Theta <- format(as.matrix(fecundity_Theta),digits=5)

  # maturity
  maturity <- read.csv(paste0(path,"/maturity_species.csv"),header=TRUE,row.names=1)
  d$maturity_nu <- unlist(maturity["nu",])
  d$maturity_omega <- unlist(maturity["omega",])
  
  maturity_covEffects <- read.csv(paste0(path,"/maturity_covariateEffects.csv"),header=TRUE)
  d$maturity_covEffects <- as.matrix(maturity_covEffects)
  
  # growth
  growth <- read.csv(paste0(path,"/growth_species.csv"),header=TRUE,row.names=1)
  d$growth_psi <- unlist(growth["psi",])
  d$growth_kappa <- unlist(growth["kappa",])
  d$growth_Linf <- unlist(growth["Linf",])
  d$growth_k <- unlist(growth["k",])
  d$growth_type <- unlist(growth["growthType",])
  
  growth_covEffects <- read.csv(paste0(path,"/growth_covariateEffects.csv"),header=TRUE)
  d$growth_covEffects <- as.matrix(growth_covEffects)
  
  # intake
  intake <- read.csv(paste0(path,"/intake_species.csv"),header=TRUE,row.names=1)
  d$intake_alpha <- unlist(intake["alpha",])
  d$intake_beta <- unlist(intake["beta",])
  
  # M1
  M1 <- read.csv(paste0(path,"/mortality_M1.csv"),header=TRUE,row.names = 1)
  d$M1 <- as.matrix(M1)
  
  #foodweb
  foodweb <- read.csv(paste0(path,"/foodweb.csv"),header=TRUE,row.names = 1)
  d$foodweb <- as.matrix(foodweb)
  
  #M2 size preference
  M2sizePref <- read.csv(paste0(path,"/mortality_M2sizePreference.csv"),header=TRUE,row.names = 1)
  d$M2sizePref_mu <- as.matrix(M2sizePref["mu",])
  d$M2sizePref_sigma <- as.matrix(M2sizePref["sigma",])
 
  #fishery/fleet selectivity
  fisherySelectivityc<- read.csv(paste0(path,"/fishing_selectivityc.csv"),header=TRUE,row.names = 1)
  d$fisherySelectivity_c <- format(as.matrix(fisherySelectivityc),digits=5)
  fisherySelectivityd<- read.csv(paste0(path,"/fishing_selectivityd.csv"),header=TRUE,row.names = 1)
  d$fisherySelectivity_d <- format(as.matrix(fisherySelectivityd),digits=5)

  # B0 - equilibrium biomass
  B0 <- read.csv(paste0(path,"/B0.csv"),header=TRUE,row.names = 1)
  d$B0 <- unlist(B0)
  
  # assessment thresholds + exploitations
  assessmentThresholds <-  read.csv(paste0(path,"/assessmentThresholds.csv"),header=TRUE)
  d$thresholds <- assessmentThresholds$thresholds
  d$Nthresholds <- length(d$thresholds)
  d$exploitationOptions <- assessmentThresholds[,2:dim(assessmentThresholds)[2]]
  
  # additionl level added to species specific threshold
  assessmentThresholdsSpecies <-  read.csv(paste0(path,"/assessmentThresholdsSpecies.csv"),header=TRUE,row.names = 1)
  d$thresholdSpecies <- unlist(assessmentThresholdsSpecies)
  
  #scaled Efort - not used
  scaledEffort <-  read.csv(paste0(path,"/observation_effortScaling.csv"),header=TRUE)
  d$scaledEffort <- unlist(scaledEffort)
  
  # discard coefficient - prob of discard
  discardCoef <-  read.csv(paste0(path,"/fishing_discards.csv"),header=TRUE,skip=3,row.names = 1)
  d$discardCoef <- (unlist(as.matrix(discardCoef)))
  
  # discard survival probability | discard
  discardSurvival <-  read.csv(paste0(path,"/fishing_discardsSurvival.csv"),header=TRUE,skip=3,row.names = 1)
  d$discardSurvival <- (unlist(as.matrix(discardSurvival)))
  return(d)
  
}

write_PinFile <- function(p,d,options){
  outputFileName <- options$outputPinFileName
  # write explanation of how this file was formed
  cat("#hydra_sim.pin for 10 species, 1 area (Georges Bank) for simulation, May 2013
      #note that species 9 and 10 have changed from ms3am test model",file=outputFileName,fill=TRUE)
  cat("# This file was created using create_DataFile.R and used all inputs from csv files found in folder:
      #createDataFiles_testing/dataInputsHydra",file=outputFileName,fill=TRUE,append=TRUE)
  
  cat("#species are based on pars from LeMANS model, see LeMANSpars_fortesting.xlsx and other lit values",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#1: spinydog",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#2: winterskate",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#3: Aherring",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#4: Acod",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#5: haddock",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#6: yellowtailfl",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#7: winterfl",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#8: Amackerel",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#9: silverhake",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#10: goosefish",file=outputFileName,fill=TRUE,append=TRUE)
  
  # year 1 initial values of N
  cat("#//Initial N year 1",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_3darray yr1N(1,Nareas,1,Nspecies,1,Nsizebins)       //initial year N at size, millions",file=outputFileName,fill=TRUE,append=TRUE)
  for (sp in 1:d$Nspecies) {
    cat(c(" ",p$Y1N[sp,]),file=outputFileName,fill=TRUE,append=TRUE)
  }
  
  # recruitment. these are not used. If we delete then all previous code not compatible.
  # When we decide to forgo backward compatibility this will be deleted.
  cat("#//recruitment parameters from .pin file (now alts by spp read in from dat file; these defaults replaced in preliminary calcs)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix recGamma_alpha(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$eggRicker_alpha),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix eggRicker_shape(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$eggRicker_shape),file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix eggRicker_beta(1,Nareas,1,Nspecies)			//eggprod gamma Ricker model alpha ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$eggRicker_beta),file=outputFileName,fill=TRUE,append=TRUE)
  
  # redundant inputs recruitment Devs etc. never used.
  cat("#  //recruitment: average annual, annual devs, actual (avg+dev)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_matrix avg_recruitment(1,Nareas,1,Nspecies,avg_rec_phase)  //average annual recruitment by area, species ",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#logspace (scaled by eye to produce flatline pops with pred mort but no fishing) ",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",p$redundantAvgRec),file=outputFileName,fill=TRUE,append=TRUE)
  
  cat("#  init_3darray recruitment_devs(1,Nareas,1,Nspecies,1,Nyrs,dev_rec_phase)  //recruitment deviations by area, species",file=outputFileName,fill=TRUE,append=TRUE)
  for (sp in 1:d$Nspecies) {
    cat(c(" ",p$redundantRecDevs[sp,]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }
  
  # recruitment sigma from S-R fits
  cat("#   init_matrix recsigma(1,Nareas,1,Nspecies)  //recruitment sigma",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",d$recSigma),file=outputFileName,fill=options$fillLength,append=TRUE)
  
  # fishery Q's
  cat("# Fishery qs",file=outputFileName,fill=TRUE,append=TRUE)
  cat("#  init_3darray fishery_q(1,Nareas,1,Nspecies,1,Nfleets,fqphase)",file=outputFileName,fill=TRUE,append=TRUE)
  cat("# area1.btrawl ptrawl longline)",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(c(" ",p$fishery_q[isp,]," #",rownames(p$fishery_q)[isp]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }

  
  # survey q
  cat("#  init_matrix survey_q(1,Nareas,1,Nspecies,sqphase)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",p$survey_q),file=outputFileName,fill=options$fillLength,append=TRUE)
  # survey sigma (observation error)
  cat("#  init_matrix surv_sigma(1,Nareas,1,Nspecies,ssig_phase)",file=outputFileName,fill=TRUE,append=TRUE)
  cat(c(" ",p$survey_sigma),file=outputFileName,fill=options$fillLength,append=TRUE)
  
  # fishery sigma (catch obs eror)
  cat("#  init_3darray catch_sigma(1,Nareas,1,Nspecies,1,Nfleets,csig_phase)",file=outputFileName,fill=TRUE,append=TRUE)
  for (isp in 1:d$Nspecies) {
    cat(c(" ",p$fishery_sigma[isp,]),file=outputFileName,fill=options$fillLength,append=TRUE)
  }
  


  
}

get_PinData <- function(options){
  # Stipulate all information required for the pin data file
  p <- list()
  # path to data
  path <- paste0(getwd(),"/",options$pathToDataFiles)
  # list of species and guilds (Functional Groups)
  Y1N <- read.csv(paste0(path,"/observation_Y1N.csv"),header=TRUE,row.names=1)
  p$Y1N <- unlist(format(as.matrix(Y1N),digits=7))
  
  # redundant Avg recruitemtn and deviations
  redundantAvgRec <- read.csv(paste0(path,"/redundantAvgRecPinData.csv"),header=TRUE,row.names=1)
  p$redundantAvgRec <- unlist(redundantAvgRec)
  redundantRecDevs <- read.csv(paste0(path,"/redundantRecDevsPinData.csv"),header=TRUE,row.names=1)
  p$redundantRecDevs <- unlist(t(as.matrix(redundantRecDevs)))
  
  
  # fishery catchability (q's)
  fisheryqs<- read.csv(paste0(path,"/fishing_q.csv"),header=TRUE,row.names = 1)
  p$fishery_q<- unlist(format(as.matrix(fisheryqs),digits=7)) 
  #p$fishery_q<- (as.table(as.matrix(fisheryqs)))

  
  # survey q and obs error
  survey<- read.csv(paste0(path,"/survey_info.csv"),header=TRUE,row.names = 1)
  p$survey_q<- unlist(survey["q",])
  p$survey_sigma<- unlist(survey["obs_error",])
  
  # fishing error
  fishery_sigma <- read.csv(paste0(path,"/fishing_error.csv"),header=TRUE,row.names = 1)
  p$fishery_sigma <- format(as.matrix(fishery_sigma),digits=NULL)
  
  return(p)
}
