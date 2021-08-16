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

