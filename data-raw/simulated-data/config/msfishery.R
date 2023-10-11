# Default fishery configuration here is a census
# June 2023 now aggregating over fleets in input
# change name to identify fleet specific config files
# output will be stored with this name

fishery.name="allfleet"

# select fleets by number from fisheries.csv Index column
# NULL is all fleets
fishfleets <- NULL

# Inherits species from input omlist_ss
fishspp <- omlist_ss$code_ss 

# fishery output: learned the hard way this can be different from ecosystem outputs
fstepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutfinc

#The last timestep to sample
total_sample <- noutsteps-1 #495

# leave out model burn in period? define how long
burnin <- 0

# take only some years? for mskeyrun we do this later, here take all
nyears <- NULL

# same time dimensioning parameters as in surveycensus.R
#Vector of indices of catch in numbers to pull (by timestep to sum)
fish_sample_full <- c(0:total_sample)  #total_sample
#fish_burnin <- burnin*fstepperyr+1
#fish_nyears <- nyears*fstepperyr
#fish_times <- fish_sample_full[fish_burnin:(fish_burnin+fish_nyears-1)]
#fish_timesteps <- seq(fish_times[fstepperyr], max(fish_times), by=fstepperyr) #last timestep
#fish_years <- unique(floor(fish_times/fstepperyr)+1) # my original
#fish_years <- unique(floor(fish_times/fstepperyr)) #from Christine's new sardine_config.R

#fishtime <- fish_times

fishtime <- fish_sample_full

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
fish_cv <- data.frame(species=fishspp, cv=rep(0.01,length(fishspp)))

