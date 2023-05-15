# what is in the CATCH.nc file?

library(here)

atlmod <- here("data-raw/simulated-data/config/NOBA_sacc38Config.R")

source(atlmod)

nc_catch <- paste0(scenario.name, 'CATCH.nc')
#nc_catch <- paste0(scenario.name, 'TOTCATCH.nc')

dir <- d.name

select_variable <- "Catch"

file.nc <- file.path(dir, nc_catch)
file_fish <- fisheries.file


# Load ATLANTIS output!
at_out <- RNetCDF::open.nc(con = file.nc)
on.exit(RNetCDF::close.nc(at_out))


# Get info from netcdf file! (Filestructure and all variable names)
var_names_ncdf <- sapply(seq_len(RNetCDF::file.inq.nc(at_out)$nvars - 1),
                         function(x) RNetCDF::var.inq.nc(at_out, x)$name)
n_timesteps <- RNetCDF::dim.inq.nc(at_out, 0)$length
n_boxes     <- RNetCDF::dim.inq.nc(at_out, 1)$length
n_layers    <- RNetCDF::dim.inq.nc(at_out, 2)$length

# To get what I hope is catch output in tons,
# keep variable names with CODES instead of species names 
# and _FC with a number corresponding to the fishery in the Fisheries.csv

if(select_variable %in% c("Catch", "Discard")){
  #input_select_variable <- select_variable
  #read in fleet names
  fleetnames <- atlantisom::load_fisheries(dir = dir, file_fish = file_fish)
  #select_variable <- concatenate select_variable "_" each fleet name
  svfish <- c()
  for(i in select_variable){
    sv <-paste0(i, "_FC", fleetnames$Index)
    svfish <- c(svfish, sv)
  }
  select_variable <- svfish
}
