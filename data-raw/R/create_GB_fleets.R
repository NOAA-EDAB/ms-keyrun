#' Assign gear to Fleets
#'
#' Uses mskeyrun results to assign gear to fleets (https://noaa-edab.github.io/ms-keyrun/)
#'
#' @return data frame of fleet name and associated gear tpyes


fleets <- data.frame(fleetName = c(rep("demersal",5),
                                   rep("fixedGear",5),
                                   rep("traps",3),
                                   rep("pelagic",3),
                                   "scallop",
                                   "surfcalm",
                                   rep("otherShellfish",4),
                                   rep("lobsterCrab",9),
                                   rep("hms",4),
                                   rep("other",10)),
                     NEGEAR2 = c(c("05","16","32","35","36"),
                                 c("01","02","10","50","52"),
                                 c("08","14","26"),
                                 c("12","17","37"),
                                 "13",
                                 "40",
                                 c("22","25","38","41"),
                                 c("20","18","30","21","23","33","15","19","53"),
                                 c("03","04","06","11"),
                                 c("28","27","99","44","31","34","81","45","09","07")
                     ),
                     hydraFleets = c(rep("demersal",5),
                                     rep("fixedGear",8),
                                     rep("pelagic",3),
                                     rep("demersal",6),
                                     rep("fixedGear",9),
                                     rep("fixedGear",4),
                                     rep("other",10)))

usethis::use_data(fleets,overwrite=T)
