# what is in the CATCH.nc file?

############# preliminaries for testing ###################
library(here)
library(stringr)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)

atlmod <- here("data-raw/simulated-data/config/NOBA_sacc38Config.R")

source(atlmod)

nc_catch <- paste0(scenario.name, 'CATCH.nc')
#nc_catch <- paste0(scenario.name, 'TOTCATCH.nc')

stepperyr <- 5

dir <- d.name

select_variable <- "Catch"
input_select_variable <- select_variable

file.nc <- file.path(dir, nc_catch)
file_fish <- fisheries.file

select_groups_name <- c("Long_rough_dab",
                   "Green_halibut",
                   "Mackerel",
                   "Haddock",
                   "Saithe",
                   "Redfish",
                   "Blue_whiting",
                   "Norwegian_ssh",
                   "North_atl_cod",
                   "Polar_cod",
                   "Capelin")

fgs <- atlantisom::load_fgs(dir = d.name, file_fgs = functional.groups.file)

# Get the boundary boxes
allboxes <- atlantisom::load_box(dir = d.name, file_bgm = box.file)
bboxes <- atlantisom::get_boundary(allboxes)


############# code to load catch by fleet in tons ###################


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

# get code matching species name and use as select groups
select_groups <- fgs$Code[which(fgs$Name %in% select_groups_name)]

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

search <- list()
for (i in seq_along(select_groups)) {
  search[[i]] <- c(
    unlist(paste0(select_groups[i], select_variable)),  # GroupVariable
    unlist(paste(select_groups[i], select_variable,
                 sep = "_"))                           # Group_Variable
  )
  search[[i]] <- search[[i]][is.element(search[[i]], var_names_ncdf)]
  search[[i]] <- unique(search[[i]])
}
search_clean <- do.call(c, search)
# If the combination of select_groups and select_variable ends up not being found.
if (length(search_clean) == 0) return(0)

at_data <- lapply(search_clean, RNetCDF::var.get.nc, ncfile = at_out)

# Get final species and number of ageclasses per species
final_species <- select_groups[sapply(
  lapply(select_groups, grepl, x = search_clean), any)]

# # Get final fleets for full age structure fishery output--wrong order
# if(input_select_variable %in% c("Catch", "Discard")){
#   final_fleet <- fleetnames$Code[sapply(
#     lapply(select_variable, grepl, x = search_clean), any)]
# }

num_layers <- RNetCDF::var.get.nc(ncfile = at_out, variable = "numlayers")[, 1]
# add sediment layer!
num_layers <- num_layers + ifelse(num_layers == 0, 0, 1)

# Create an array of layerids.
# Every entry in the array indicates if a layer is present (= 1) or not (= 0).
# Boxes without layers (= islands) have only 0s as id,
# used later on to remove data from non-existent layers!
# By default output should be 0 in those layers.
# Layers in boundary boxes are set to 0 if bboxes is anything other than NULL!
# Applying a boolean array to an array results in a vector!
for (i in seq_along(num_layers)) {
  if (i == 1) layerid <- array(dim = c(n_layers, n_boxes))
  if (num_layers[i] == 0) {
    layerid[, i] <- 0
  } else {
    if (!is.null(bboxes) & is.element((i - 1), bboxes)) {
      layerid[, i] <- 0
    } else {
      layerid[, i] <- c(rep(0, times = n_layers - num_layers[i]),
                        rep(1, times = num_layers[i]))
    }
  }
}

# Create vectors for polygons and layers
# Each vector has the length equal to one time-step
# All data from islands and non-existent layers is removed
# Therefore the length of these
# vectors is equal for each extracted variable
boxes <- 0:(n_boxes - 1)
# Remove islands and boundary boxes
island_ids <- num_layers == 0
if (!is.null(bboxes)) {
  boundary_ids <- is.element(boxes, bboxes)
  island_ids <- island_ids | boundary_ids
}
boxes <- boxes[!island_ids]
num_layers <- num_layers[!island_ids]

polygons <- rep(boxes, times = num_layers)
layers <- sapply(num_layers[num_layers != 0] - 2,
                 function(x) c(seq(x, from = 0, by = 1), n_layers - 1))
if (any(sapply(layers, length) != num_layers[num_layers != 0])) {
  stop("Number of layers incorrect.")
}
layers <- do.call(c, layers)
if (length(polygons) != length(layers)) {
  stop("Number of polygons and layers do not match.")
}

# In the following section the data is transformed to a long dataframe
# I haven't found any solution to vectorize the creation of the dataframe
# columns (species, age, polygons,...)
# when data from 2d and 3d arrays
# (e.g. select_variable = "N" all biomasspools are only present in the
# sediment layer.) are read in simultaneously.
# Therefore the current "messy" solution splits the data
# in 2 subpopulations: 2d-data and 3d-data
at_data3d <- at_data[which(sapply(at_data, function(x) length(dim(x))) == 3)]
at_data2d <- at_data[which(sapply(at_data, function(x) length(dim(x))) == 2)]

int_fs <- final_species
int_fa <- rep(1, length(final_species))

if(input_select_variable %in% c("Catch", "Discard")){
  # how many unique fleets per select_group in search_clean?
  # lookup of species and fleet names
  splitfleets <- search_clean %>%
    stringr::str_replace(paste0("_",input_select_variable,"_FC"), "-")
  sp_fleet <- as.data.frame(unique(splitfleets)) %>%
    dplyr::rename(spfleet = "unique(splitfleets)") %>%
    tidyr::separate(spfleet, c("species", "fleet"), sep = "-")
  # lookup of number of fleets per species
  sp_nfleet <- as.data.frame(table(sp_fleet$species)) %>%
    dplyr::rename(species = Var1, nfleet = Freq)
  final_fleet <- sp_nfleet[match(final_species, sp_nfleet$species),]
  # for indexing fleets
  int_ff <- final_fleet$nfleet
}

# 3d should not apply here
# if (length(at_data3d) >= 1) {
#   # Remove biomasspools if selected variable is "N"!
#   if (input_select_variable == "N") {
#     int_fs <- final_species[!is.element(final_species, bps)]
#     int_fa <- final_agecl[!is.element(final_species, bps)]
#     # Note this only works if age-structured vertebrates have 10 ageclasses
#     # there is no "N" output in annage files so has no impact
#     int_fa[int_fa == 10] <- 1
#   }
#   for (i in seq_along(at_data3d)) {# for loop over all variables
#     if (i == 1) result3d <- list()
#     for (j in 1:n_timesteps) {# loop over timesteps
#       if (j == 1) values <- array(dim = c(length(layers), n_timesteps))
#       values[, j] <- at_data3d[[i]][,, j][layerid == 1]
#     }
#     result3d[[i]] <- as.vector(values)
#   }
#   result3d <- data.frame(
#     species = unlist(sapply(
#       X = mapply(FUN = rep, x = int_fs, each = int_fa, SIMPLIFY = FALSE,
#                  USE.NAMES = FALSE),
#       FUN = rep, each = length(layers) * n_timesteps, simplify = FALSE)),
#     agecl = unlist(sapply(
#       X = sapply(X = int_fa, FUN = seq, from = 1, by = 1, simplify = FALSE,
#                  USE.NAMES = FALSE),
#       FUN = rep, each = length(layers) * n_timesteps, simplify = FALSE)),
#     polygon = unlist(sapply(
#       X = n_timesteps * int_fa, FUN = rep, x = polygons, simplify = F,
#       USE.NAMES = FALSE)),
#     layer = unlist(sapply(
#       X = n_timesteps * int_fa, FUN = rep, x = layers, simplify = FALSE,
#       USE.NAMES = FALSE)),
#     time = unlist(sapply(
#       X = int_fa, FUN = rep, x = rep(0:(n_timesteps - 1), each = length(layers)),
#       simplify = FALSE, USE.NAMES = FALSE)),
#     atoutput = do.call(c, result3d),
#     stringsAsFactors = FALSE)
# }

if (length(at_data2d) >= 1) {
  # # Only select biomasspools if selected variable is "N"!
  # if (input_select_variable == "N") {
  #   int_fs <- final_species[is.element(final_species, bps)]
  #   int_fa <- final_agecl[is.element(final_species, bps)]
  # }
  # # age-structured invert groups are combined in ncdf file!
  # if (input_select_variable == "Grazing") int_fa <- 1

   for (i in seq_along(at_data2d)) {# for loop over all variables!
     if (i == 1) result2d <- list()
     for (j in 1:n_timesteps) {# loop over timesteps
       if (j == 1) values <- array(dim = c(length(boxes), n_timesteps))
       values[, j] <- at_data2d[[i]][, j][boxes + 1]
     }
     result2d[[i]] <- as.vector(values)
  }
  
  # Order of the data in value column = "atoutput".
  # 1. species  --> rep each with the number of
  #                 ageclasses * fleets and n_timesteps * boxes
  # 2. age      --> rep each (1:maxage for each species) with n_timesteps * boxes
  # 3. timestep --> rep each timestep (1:n_timesteps)
  #                 with the number of boxes and final_agecl
  #                 (num ages per species)
  # 4. polygon  --> rep boxes times n_timesteps * final_agecl
  #                 (num ages per species)
  # 5. fleet    -->
  
  # if(input_select_variable %in% c("Nums", "Weight")){
  #   
  #   result2d <- data.frame(species = unlist(sapply(
  #     X = mapply(FUN = rep, x = int_fs, each = int_fa, SIMPLIFY = FALSE,
  #                USE.NAMES = FALSE),
  #     FUN = rep, each = length(boxes) * n_timesteps, simplify = FALSE)),
  #     agecl = unlist(sapply(X = sapply(X = int_fa, FUN = seq, from = 1,
  #                                      by = 1, simplify = FALSE, USE.NAMES = FALSE),
  #                           FUN = rep, each = length(boxes) * n_timesteps, simplify = FALSE)),
  #     polygon = unlist(sapply(X = n_timesteps * int_fa,
  #                             FUN = rep, x = boxes, simplify = FALSE, USE.NAMES = FALSE)),
  #     time = unlist(sapply(X = int_fa, FUN = rep, x = rep(0:(n_timesteps - 1),
  #                                                         each = length(boxes)), simplify = FALSE, USE.NAMES = FALSE)),
  #     atoutput = do.call(c, result2d),
  #     stringsAsFactors = F)
  #   if (select_variable == "N") result2d$layer <- n_layers - 1
  # }
  
  #should now work properly for multiple fleets
  if(input_select_variable %in% c("Catch", "Discard")){
    result2d <- data.frame(species = unlist(sapply(X = mapply(FUN = rep, x = int_fs,
                                                              each = (int_fa * int_ff),SIMPLIFY = FALSE,USE.NAMES = FALSE),
                                                   FUN = rep, each = length(boxes) * n_timesteps, simplify = FALSE)),
                           agecl = unlist(sapply(X = sapply(X = rep(int_fa, int_ff), FUN = seq, from = 1,
                                                            by = 1, simplify = FALSE, USE.NAMES = FALSE),
                                                 FUN = rep, each = (length(boxes) * n_timesteps), simplify = FALSE)),
                           polygon = unlist(sapply(X = n_timesteps * int_fa * int_ff,
                                                   FUN = rep, x = boxes, simplify = FALSE, USE.NAMES = FALSE)),
                           fleet = unlist(sapply(X = mapply(FUN = rep, x = sp_fleet$fleet,
                                                            each = (rep(int_fa, int_ff)),SIMPLIFY = FALSE,USE.NAMES = FALSE),
                                                 FUN = rep, each = (length(boxes) * n_timesteps), simplify = FALSE)),
                           time = unlist(sapply(X = int_fa * int_ff , FUN = rep, x = rep(0:(n_timesteps - 1),
                                                                                         each = length(boxes)), simplify = FALSE, USE.NAMES = FALSE)),
                           atoutput = do.call(c, result2d),
                           stringsAsFactors = F)
    #if (select_variable == "N") result2d$layer <- n_layers - 1
  }
  
}

# Combine dataframes if necessary!
# if (all(sapply(lapply(at_data, dim), length) == 3) & input_select_variable != "N") {
#   result <- result3d
# }
if (all(sapply(lapply(at_data, dim), length) == 2) & input_select_variable != "N") {
  result <- result2d
}
if (input_select_variable == "N") {
  if (length(at_data2d) >= 1 & length(at_data3d) == 0) result <- result2d
  if (length(at_data2d) == 0 & length(at_data3d) >= 1) result <- result3d
  if (length(at_data2d) >= 1 & length(at_data3d) >= 1) {
    result <- rbind(result2d, result3d)
  }
}

##### aggregate over polygons to fleets #######

aggcatchtons <- result %>%
  dplyr::select(-agecl) %>%
  dplyr::group_by(species, fleet, time) %>%
  dplyr::summarise(totcatch = sum(atoutput)) %>%
  dplyr::mutate(year = ceiling(time/stepperyr)) 

# compare annual catch in tons to CATCH.txt output
yearcatchtons <- aggcatchtons %>%
  dplyr::group_by(species, year) %>%
  dplyr::summarise(yeartot = sum(totcatch))

txtCatchtons <- atlantisom::load_catch(d.name, catch.file, fgs) %>%
  dplyr::filter(species %in% select_groups_name) %>%
  dplyr::mutate(year = time/365) %>%
  dplyr::left_join(dplyr::select(fgs, Code, Name), by = c("species" = "Name"))

comparetons <- txtCatchtons %>%
  dplyr::left_join(yearcatchtons, by = c("Code" = "species", "year" = "year")) %>%
  ggplot2::ggplot() +
  geom_line(aes(year, atoutput)) +
  geom_point(aes(year, yeartot), color = "blue", alpha = 0.2) +
  facet_wrap(~species, scales="free_y")
 
# looks like a match!               
comparetons
