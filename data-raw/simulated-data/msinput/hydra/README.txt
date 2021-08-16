Simulated data from Norweigan-Barents Atlantis Model by Hansen et al. 

 Atlantis outputs stored in /Users/sgaichas/Documents/0_Data/ms-keyrun/simulated-data/atlantisoutput/NOBA_march_2020 

 Biomass output for surveys BTS_fall_allbox_effic1 

 Biomass output for surveys BTS_fall_nearbox_qmix_selmix 

 Biomass output for surveys BTS_spring_allbox_effic1 

 Biomass output for surveys BTS_spring_nearbox_qmix_selmix 

 Fishery catch output for fisheries census 

 Fishery catch output for fisheries nearbox 

 Biomass and catch output units are annual tons.

 Survey specifications:
 ----------------------
 # Default survey configuration here has a range of efficiencies and selectivities
# To emulate a range of species in a single multispecies survey
# Also now happens in "spring" and "fall"
# Need to define survey season, area, efficiency, selectivity

# Survey name
survey.name="BTS_fall_allbox_effic1"

#Atlantis model timestep corresponding to the true output--now from census_spec.R
timestep <- stepperyr #5

#Which atlantis timestep does the survey run in?--now from census_spec.R
# with 5 output steps per year, 0 is Jan-Feb-midMar, 1 is midMar-Apr-May, 
# 2 is June-July-midAug, 3 is midAug-Sept-Oct, 4 is Nov-Dec (ish)

survey_sample_time <- 3 # fall survey

#The last timestep to sample
total_sample <- noutsteps-1 #495

#Vector of indices of survey times to pull
survey_sample_full <- seq(survey_sample_time,
                          total_sample, by=timestep)

survtime <- survey_sample_full

# survey area
# should return all model areas
survboxes <- allboxes

# survey efficiency (q)
# should return a perfectly efficient survey 
surveffic <- data.frame(species=survspp,
                     efficiency=rep(1.0,length(survspp)))

# survey selectivity (agecl based)
# this is by age class, need to change to use with ANNAGEBIO output
#survselex <- data.frame(species=rep(survspp, each=n_age_classes),
#                     agecl=rep(c(1:n_age_classes),length(survspp)),
#                     selex=rep(1.0,length(survspp)*n_age_classes))

# for annage output
survselex <- data.frame(species=rep(names(annages), n_annages), #  
                        agecl=unlist(sapply(n_annages,seq)),
                        selex=rep(1.0,sum(n_annages)))

survselex.agecl <- survselex 


# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
surveffN <- data.frame(species=survspp, effN=rep(100000, length(survspp)))

# survey index cv needed for sample_survey_xxx
# cv = 0.1
surv_cv <- data.frame(species=survspp, cv=rep(0.1,length(survspp)))

# length at age cv for input into calc_age2length function
# function designed to take one cv for all species, need to change to pass it a vector
lenage_cv <- 0.1

# max size bin for length estimation, function defaults to 150 cm if not supplied
maxbin <- 200

# diet sampling parameters
alphamult <- 10000000
unidprey <- 0
 
 ----------------------
 # Default survey configuration here has a range of efficiencies and selectivities
# To emulate a range of species in a single multispecies survey
# Also now happens in "spring" and "fall"
# Need to define survey season, area, efficiency, selectivity

# Survey name
survey.name="BTS_spring_allbox_effic1"

#Atlantis model timestep corresponding to the true output--now from census_spec.R
timestep <- stepperyr #5

#Which atlantis timestep does the survey run in?--now from census_spec.R
# with 5 output steps per year, 0 is Jan-Feb-midMar, 1 is midMar-Apr-May, 
# 2 is June-July-midAug, 3 is midAug-Sept-Oct, 4 is Nov-Dec (ish)

survey_sample_time <- 1 # spring survey

#The last timestep to sample
total_sample <- noutsteps-1 #495

#Vector of indices of survey times to pull
survey_sample_full <- seq(survey_sample_time,
                          total_sample, by=timestep)

survtime <- survey_sample_full

# survey area
# should return all model areas
survboxes <- allboxes

# survey efficiency (q)
# should return a perfectly efficient survey 
surveffic <- data.frame(species=survspp,
                     efficiency=rep(1.0,length(survspp)))

# survey selectivity (agecl based)
# this is by age class, need to change to use with ANNAGEBIO output
#survselex <- data.frame(species=rep(names(age_classes), each=n_age_classes),
#                     agecl=rep(c(1:n_age_classes),length(survspp)),
#                     selex=rep(1.0,length(survspp)*n_age_classes))

# for annage output uses names(annages) NOT alphabetical survspp
survselex <- data.frame(species=rep(names(annages), n_annages), #  
                        agecl=unlist(sapply(n_annages,seq)),
                        selex=rep(1.0,sum(n_annages)))

survselex.agecl <- survselex


# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
surveffN <- data.frame(species=survspp, effN=rep(100000, length(survspp)))

# survey index cv needed for sample_survey_xxx
# cv = 0.1
surv_cv <- data.frame(species=survspp, cv=rep(0.1,length(survspp)))

# length at age cv for input into calc_age2length function
# function designed to take one cv for all species, need to change to pass it a vector
lenage_cv <- 0.1

# max size bin for length estimation, function defaults to 150 cm if not supplied
maxbin <- 200

# diet sampling parameters
alphamult <- 10000000
unidprey <- 0


 
 ----------------------
 # Default survey configuration here has a range of efficiencies and selectivities
# To emulate a range of species in a single multispecies survey
# Also now happens in "spring" and "fall"
# Need to define survey season, area, efficiency, selectivity

# Survey name
survey.name="BTS_fall_nearbox_qmix_selmix"

#Atlantis model timestep corresponding to the true output--now from census_spec.R
timestep <- stepperyr #5

#Which atlantis timestep does the survey run in?--now from census_spec.R
# with 5 output steps per year, 0 is Jan-Feb-midMar, 1 is midMar-Apr-May, 
# 2 is June-July-midAug, 3 is midAug-Sept-Oct, 4 is Nov-Dec (ish)

survey_sample_time <- 3 # fall survey

#The last timestep to sample
total_sample <- noutsteps-1 #495

#Vector of indices of survey times to pull
survey_sample_full <- seq(survey_sample_time,
                          total_sample, by=timestep)

survtime <- survey_sample_full

# survey area
# should return all model areas
#survboxes <- allboxes
# shelf boxes excluding farthest east Barents
survboxes <- c(1:8, 21, 23:31, 33, 39:44, 47:49)

# survey efficiency (q)
# should return a perfectly efficient survey 
# surveffic <- data.frame(species=survspp,
#                     efficiency=rep(1.0,length(survspp)))

# group species to apply different efficiency (all NOBA fish and shark groups)
nontrawl <- c("Sharks_other", "Pelagic_large","Mesop_fish")
pelagics <- c("Pelagic_small","Redfish_other","Mackerel","Haddock",
              "Saithe","Redfish","Blue_whiting","Norwegian_ssh","Capelin")
demersals <- c("Demersals_other","Demersal_large","Flatfish_other","Skates_rays",
               "Green_halibut","North_atl_cod","Polar_cod","Snow_crab")
selflats <- c("Long_rough_dab")

#   define bottom trawl mixed efficiency
ef.nt <- 0.01 # for large pelagics, reef dwellers, others not in trawlable habitat
ef.pl <- 0.1  # for pelagics
ef.dm <- 0.6  # for demersals
ef.fl <- 1.1  # for selected flatfish

# bottom trawl survey efficiency specification by species group
effnontrawl <- data.frame(species=nontrawl, efficiency=rep(ef.nt,length(nontrawl)))
effpelagics <- data.frame(species=pelagics, efficiency=rep(ef.pl,length(pelagics)))
effdemersals <- data.frame(species=demersals, efficiency=rep(ef.dm,length(demersals)))
effselflats <- data.frame(species=selflats, efficiency=rep(ef.fl,length(selflats)))

efficmix <- bind_rows(effnontrawl, effpelagics, effdemersals, effselflats)

surveffic <- efficmix %>%
  filter(species %in% survspp)

# survey selectivity (true age based, flat)
# for annage output uses names(annages) NOT alphabetical survspp
survselex <- data.frame(species=rep(names(annages), n_annages), #  
                        agecl=unlist(sapply(n_annages,seq)),
                        selex=rep(1.0,sum(n_annages)))

#   mixed selectivity: specify for annual ages 0-10 to 
#   then apply that curve to agecl keeping every nth selectivity index n=NumAgeClassSize
#   ageclsel <- fullsel[seq(NumAgeClassSize, n_annages, length.out=NumCohorts)]
#     flat=1 for large pelagics, reef dwellers, others not in trawlable habitat
#     sigmoid 0 to 1 with 0.5 inflection ~ age 3 for pelagics, reaching 1 at age 5, flat top
#     sigmoid 0 to 1 with 0.5 inflection ~ age 5 for most demersals and flatfish, reaching 1 at age 7, flat top
#     dome shaped 0 to 1 at agecl 6&7 for selected demersals, falling off to 0.7 by agecl 10--didn't do

sigmoid <- function(a,b,x) {
  1 / (1 + exp(-a-b*x))
}

sp_age <- sp_age %>%
  mutate(n_annages = NumCohorts * NumAgeClassSize) 
# 
# survey selectivity specification for true ages 1-10 by species group--replace for each group in surselex

selnontrawl <- data.frame(species=rep(nontrawl, each=10),
                          agecl=rep(c(1:10),length(nontrawl)),
                          selex=rep(1.0,length(nontrawl)*10))
selpelagics <- data.frame(species=rep(pelagics, each=10),
                          agecl=rep(c(1:10),length(pelagics)),
                          selex=sigmoid(5,1,seq(-10,10,length.out=10)))
seldemersals <- data.frame(species=rep(demersals, each=10),
                           agecl=rep(c(1:10),length(demersals)),
                           selex=sigmoid(1,1,seq(-10,10,length.out=10)))
selselflats <- data.frame(species=rep(selflats, each=10),
                          agecl=rep(c(1:10),length(selflats)),
                          selex=sigmoid(1,1,seq(-10,10,length.out=10)))

selexmix <- bind_rows(selnontrawl, selpelagics, seldemersals, selselflats) 

selexmix <- selexmix %>%
  filter(species %in% survspp) %>%
  rename(selex10 = selex)

survselex <- merge(survselex, selexmix, all = TRUE) %>%
  filter(!is.na(selex)) %>%
  mutate(selex = case_when(!is.na(selex10) ~ selex10,
                           is.na(selex10) ~ selex)) %>%
  select(-selex10)

# now apply this for agecl selectivity that matches
# and figure out how to use in wrapper!
#ageclsel <- fullsel[seq(NumAgeClassSize, n_annages, length.out=NumCohorts)]

survselex.agecl <- survselex %>% left_join(sp_age, by=c("species"="Name")) %>%
  group_by(species) %>%
  filter(agecl %in% seq(unique(NumAgeClassSize), 
                        unique(n_annages), 
                        length.out=unique(NumCohorts))) %>%
  mutate(agecl = agecl/unique(NumAgeClassSize)) %>%
  select(species, agecl, selex)

# effective sample size needed for sample_fish
# this is the number of *lengths* per species that are measured on a survey
# effective N of 100000 with no age subsample matches true age comp
surveffN <- data.frame(species=survspp, effN=rep(10000, length(survspp)))

# survey index cv needed for sample_survey_xxx
# cv = 0.1
# surv_cv <- data.frame(species=survspp, cv=rep(0.1,length(survspp)))

# use this constant 0 cv for testing
surv_cv_0 <- data.frame(species=survspp, cv=rep(0.0,length(survspp)))

#   define bottom trawl survey cv by group
cv.nt <- 1.0 # for large pelagics, reef dwellers, others not in trawlable habitat
cv.pl <- 0.4  # for pelagics
cv.dm <- 0.2  # for demersals
cv.fl <- 0.2  # for selected flatfish

# specify cv by species groups
surv_cv_nontrawl <- data.frame(species=nontrawl, cv=rep(cv.nt,length(nontrawl)))
surv_cv_pelagics <- data.frame(species=pelagics, cv=rep(cv.pl,length(pelagics)))
surv_cv_demersals <- data.frame(species=demersals, cv=rep(cv.dm,length(demersals)))
surv_cv_selflats <- data.frame(species=selflats, cv=rep(cv.fl,length(selflats)))

surv_cv_mix <- bind_rows(surv_cv_nontrawl, surv_cv_pelagics, surv_cv_demersals, surv_cv_selflats)

surv_cv <- surv_cv_mix %>%
  filter(species %in% survspp)

# length at age cv for input into calc_age2length function
# function designed to take one cv for all species, need to change to pass it a vector
lenage_cv <- 0.1

# max size bin for length estimation, function defaults to 150 cm if not supplied
maxbin <- 200

# diet sampling parameters
alphamult <- 10
unidprey <- 0.3

 
 ----------------------
 # Default survey configuration here has a range of efficiencies and selectivities
# To emulate a range of species in a single multispecies survey
# Also now happens in "spring" and "fall"
# Need to define survey season, area, efficiency, selectivity

# Survey name
survey.name="BTS_spring_nearbox_qmix_selmix"

#Atlantis model timestep corresponding to the true output--now from census_spec.R
timestep <- stepperyr #5

#Which atlantis timestep does the survey run in?--now from census_spec.R
# with 5 output steps per year, 0 is Jan-Feb-midMar, 1 is midMar-Apr-May, 
# 2 is June-July-midAug, 3 is midAug-Sept-Oct, 4 is Nov-Dec (ish)

survey_sample_time <- 1 # spring survey

#The last timestep to sample
total_sample <- noutsteps-1 #495

#Vector of indices of survey times to pull
survey_sample_full <- seq(survey_sample_time,
                          total_sample, by=timestep)

survtime <- survey_sample_full

# survey area
# should return all model areas
# survboxes <- allboxes

# shelf boxes excluding farthest east Barents
survboxes <- c(1:8, 21, 23:31, 33, 39:44, 47:49)

# survey efficiency (q)
# should return a perfectly efficient survey 
# surveffic <- data.frame(species=survspp,
#                     efficiency=rep(1.0,length(survspp)))

# group species to apply different efficiency (all NOBA fish and shark groups)
nontrawl <- c("Sharks_other", "Pelagic_large","Mesop_fish")
pelagics <- c("Pelagic_small","Redfish_other","Mackerel","Haddock",
              "Saithe","Redfish","Blue_whiting","Norwegian_ssh","Capelin")
demersals <- c("Demersals_other","Demersal_large","Flatfish_other","Skates_rays",
               "Green_halibut","North_atl_cod","Polar_cod","Snow_crab")
selflats <- c("Long_rough_dab")

#   define bottom trawl mixed efficiency
ef.nt <- 0.01 # for large pelagics, reef dwellers, others not in trawlable habitat
ef.pl <- 0.2  # for pelagics
ef.dm <- 0.7  # for demersals
ef.fl <- 1.1  # for selected flatfish

# bottom trawl survey efficiency specification by species group
effnontrawl <- data.frame(species=nontrawl, efficiency=rep(ef.nt,length(nontrawl)))
effpelagics <- data.frame(species=pelagics, efficiency=rep(ef.pl,length(pelagics)))
effdemersals <- data.frame(species=demersals, efficiency=rep(ef.dm,length(demersals)))
effselflats <- data.frame(species=selflats, efficiency=rep(ef.fl,length(selflats)))

efficmix <- bind_rows(effnontrawl, effpelagics, effdemersals, effselflats)

surveffic <- efficmix %>%
  filter(species %in% survspp)

# survey selectivity (true age based, flat)
# for annage output uses names(annages) NOT alphabetical survspp
 survselex <- data.frame(species=rep(names(annages), n_annages), #  
                         agecl=unlist(sapply(n_annages,seq)),
                         selex=rep(1.0,sum(n_annages)))

#   mixed selectivity: specify for annual ages 0-10 to 
#   then apply that curve to agecl keeping every nth selectivity index n=NumAgeClassSize
#   ageclsel <- fullsel[seq(NumAgeClassSize, n_annages, length.out=NumCohorts)]
#     flat=1 for large pelagics, reef dwellers, others not in trawlable habitat
#     sigmoid 0 to 1 with 0.5 inflection ~ age 3 for pelagics, reaching 1 at age 5, flat top
#     sigmoid 0 to 1 with 0.5 inflection ~ age 5 for most demersals and flatfish, reaching 1 at age 7, flat top
#     dome shaped 0 to 1 at agecl 6&7 for selected demersals, falling off to 0.7 by agecl 10--didn't do

sigmoid <- function(a,b,x) {
  1 / (1 + exp(-a-b*x))
}

 sp_age <- sp_age %>%
   mutate(n_annages = NumCohorts * NumAgeClassSize) 
# 
# survey selectivity specification for true ages 1-10 by species group--replace for each group in surselex

selnontrawl <- data.frame(species=rep(nontrawl, each=10),
                          agecl=rep(c(1:10),length(nontrawl)),
                          selex=rep(1.0,length(nontrawl)*10))
selpelagics <- data.frame(species=rep(pelagics, each=10),
                          agecl=rep(c(1:10),length(pelagics)),
                          selex=sigmoid(5,1,seq(-10,10,length.out=10)))
seldemersals <- data.frame(species=rep(demersals, each=10),
                           agecl=rep(c(1:10),length(demersals)),
                           selex=sigmoid(1,1,seq(-10,10,length.out=10)))
selselflats <- data.frame(species=rep(selflats, each=10),
                          agecl=rep(c(1:10),length(selflats)),
                          selex=sigmoid(1,1,seq(-10,10,length.out=10)))

selexmix <- bind_rows(selnontrawl, selpelagics, seldemersals, selselflats) 

selexmix <- selexmix %>%
  filter(species %in% survspp) %>%
  rename(selex10 = selex)

survselex <- merge(survselex, selexmix, all = TRUE) %>%
  filter(!is.na(selex)) %>%
  mutate(selex = case_when(!is.na(selex10) ~ selex10,
                           is.na(selex10) ~ selex)) %>%
  select(-selex10)
  
# now apply this for agecl selectivity that matches
# and figure out how to use in wrapper!
#ageclsel <- fullsel[seq(NumAgeClassSize, n_annages, length.out=NumCohorts)]

survselex.agecl <- survselex %>% left_join(sp_age, by=c("species"="Name")) %>%
  group_by(species) %>%
  filter(agecl %in% seq(unique(NumAgeClassSize), 
                        unique(n_annages), 
                        length.out=unique(NumCohorts))) %>%
  mutate(agecl = agecl/unique(NumAgeClassSize)) %>%
  select(species, agecl, selex)

# effective sample size needed for sample_fish
# this is the number of *lengths* per species that are measured on a survey
# effective N of 100000 with no age subsample matches true age comp
surveffN <- data.frame(species=survspp, effN=rep(10000, length(survspp)))

# survey index cv needed for sample_survey_xxx
# cv = 0.1
# surv_cv <- data.frame(species=survspp, cv=rep(0.1,length(survspp)))

# use this constant 0 cv for testing
surv_cv_0 <- data.frame(species=survspp, cv=rep(0.0,length(survspp)))

#   define bottom trawl survey cv by group
cv.nt <- 1.0 # for large pelagics, reef dwellers, others not in trawlable habitat
cv.pl <- 0.4  # for pelagics
cv.dm <- 0.2  # for demersals
cv.fl <- 0.2  # for selected flatfish

# specify cv by species groups
surv_cv_nontrawl <- data.frame(species=nontrawl, cv=rep(cv.nt,length(nontrawl)))
surv_cv_pelagics <- data.frame(species=pelagics, cv=rep(cv.pl,length(pelagics)))
surv_cv_demersals <- data.frame(species=demersals, cv=rep(cv.dm,length(demersals)))
surv_cv_selflats <- data.frame(species=selflats, cv=rep(cv.fl,length(selflats)))

surv_cv_mix <- bind_rows(surv_cv_nontrawl, surv_cv_pelagics, surv_cv_demersals, surv_cv_selflats)

surv_cv <- surv_cv_mix %>%
  filter(species %in% survspp)

# length at age cv for input into calc_age2length function
# function designed to take one cv for all species, need to change to pass it a vector
lenage_cv <- 0.1

# max size bin for length estimation, function defaults to 150 cm if not supplied
maxbin <- 200

# diet sampling parameters
alphamult <- 10
unidprey <- 0.3

 

 Fishery specifications:
 ----------------------
 # Default fishery configuration here is a census
fishery.name="census"

# Inherits species from input omlist_ss
fishspp <- omlist_ss$species_ss 

#Number of years of data to pull
nyears <- 50

#Atlantis initialization period in years
burnin <- 30

# fishery output: learned the hard way this can be different from ecosystem outputs
fstepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutfinc


# same time dimensioning parameters as in surveycensus.R
#Vector of indices of catch in numbers to pull (by timestep to sum)
fish_sample_full <- c(0:total_sample)  #total_sample defined in sardinesurvey.R
fish_burnin <- burnin*fstepperyr+1
fish_nyears <- nyears*fstepperyr
fish_times <- fish_sample_full[fish_burnin:(fish_burnin+fish_nyears-1)]
fish_timesteps <- seq(fish_times[fstepperyr], max(fish_times), by=fstepperyr) #last timestep
#fish_years <- unique(floor(fish_times/fstepperyr)+1) # my original
fish_years <- unique(floor(fish_times/fstepperyr)) #from Christine's new sardine_config.R

fishtime <- fish_times


# fishery sampling area
# should return all model areas, this assumes you see everything that it caught
fishboxes <- c(0:(omlist_ss$boxpars$nbox - 1))

# effective sample size needed for sample_fish
# this effective N is divided by the number of annual timesteps below, so 200 per time
# use as input to the length samples, ages can be a subset
fisheffN <- data.frame(species=survspp, effN=rep(1000, length(survspp)))

#this adjusts for subannual fishery output so original effN is for whole year
fisheffN$effN <- fisheffN$effN/fstepperyr 

# fishery catch cv can be used in sample_survey_biomass
# perfect observation
fish_cv <- data.frame(species=survspp, cv=rep(0.01,length(survspp)))

 
 ----------------------
 # Default fishery configuration here is a census
fishery.name="nearbox"

# Inherits species from input omlist_ss
fishspp <- omlist_ss$species_ss 

#Number of years of data to pull
nyears <- 50

#Atlantis initialization period in years
burnin <- 30

# fishery output: learned the hard way this can be different from ecosystem outputs
fstepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutfinc


# same time dimensioning parameters as in surveycensus.R
#Vector of indices of catch in numbers to pull (by timestep to sum)
fish_sample_full <- c(0:total_sample)  #total_sample defined in survey configs
fish_burnin <- burnin*fstepperyr+1
fish_nyears <- nyears*fstepperyr
fish_times <- fish_sample_full[fish_burnin:(fish_burnin+fish_nyears-1)]
fish_timesteps <- seq(fish_times[fstepperyr], max(fish_times), by=fstepperyr) #last timestep
#fish_years <- unique(floor(fish_times/fstepperyr)+1) # my original
fish_years <- unique(floor(fish_times/fstepperyr)) #from Christine's new sardine_config.R

fishtime <- fish_times


# fishery sampling area (applies to comps, not total catch)
# should return all model areas, this assumes you see everything that is caught
#fishboxes <- c(0:(omlist_ss$boxpars$nbox - 1))
# same as survey, only getting catch comps from nearby boxes
fishboxes <- c(1:8, 21, 23:31, 33, 39:44, 47:49)

# effective sample size needed for sample_fish
# based on fishery lengths in Northeast US, my original effN of 1000 was too high
# this effective N is divided by the number of annual timesteps below, so 200 per time
# use as input to the length samples, ages can be a subset
fisheffN <- data.frame(species=survspp, effN=rep(500, length(survspp)))

#this adjusts for subannual fishery output so original effN is for whole year
fisheffN$effN <- fisheffN$effN/fstepperyr 

# fishery catch cv can be used in sample_fishery_totcatch to
# perfect observation is cv=0
fish_cv <- data.frame(species=survspp, cv=rep(0.1,length(survspp)))

 
