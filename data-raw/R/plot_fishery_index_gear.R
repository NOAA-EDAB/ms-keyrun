library(ggplot2)
library(magrittr)

focalspp <- mskeyrun::focalSpecies %>%
  dplyr::filter(modelName != "Pollock") %>% # not using in these models
  dplyr::mutate(Name = modelName,
                NESPP3 = as.integer(NESPP3)) %>%
  dplyr::distinct()

catchIndexGear <- catchIndexGear %>%
  dplyr::left_join(focalspp) %>%
  dplyr::filter(!is.na(Name))

ggplot(catchIndexGear, aes(x=YEAR, y=value, fill=Fleet)) +
   geom_bar(stat = "identity") +
   facet_wrap(Name~variable, scales = "free_y")
   #facet_wrap(~Name, scales = "free_y")

catchIndex <- mskeyrun::catchIndex %>%
  dplyr::left_join(focalspp) %>%
  dplyr::filter(!is.na(Name))

# current aggregated catch data
ggplot(catchIndex, aes(x=YEAR, y=value, fill=variable)) +
  geom_bar(stat = "identity") +
  #facet_wrap(Name~variable, scales = "free_y")
  facet_wrap(~Name, scales = "free_y")

# test proportion by non NA gear
ggplot(propgear, aes(x=YEAR, y=prop, fill=Fleet)) +
  geom_bar(stat = "identity") +
  facet_wrap(modelName~variable, scales = "free_y")

# see filled values (done before aggregating to )
ggplot(catchIndexComlandrGearNAfill, aes(x=YEAR, y=valueNAfill, fill=Fleet)) +
  geom_bar(stat = "identity") +
  facet_wrap(modelName~variable, scales = "free_y")
#facet_wrap(~Name, scales = "free_y")

# see aggregated values to hydraFleets
ggplot(catchIndexGearNAfill, aes(x=YEAR, y=valueNAfill, fill=hydraFleets)) +
  geom_bar(stat = "identity") +
  facet_wrap(modelName~variable, scales = "free_y")

# see total aggregated values to hydraFleets
ggplot(catchIndexGearNAfill, aes(x=YEAR, y=valueNAfill, fill=hydraFleets)) +
  geom_bar(stat = "identity") +
  facet_wrap(~modelName, scales = "free_y")

# scale total aggregated values to hydraFleets in model timeframe
ggplot(catchIndexGearNAfill |> dplyr::filter(YEAR > 1977), 
       aes(x=YEAR, y=valueNAfill, fill=hydraFleets)) +
  geom_bar(stat = "identity") +
  facet_wrap(~modelName, scales = "free_y")

# landings only for target fishery definition
ggplot(catchIndexGearNAfill |> dplyr::filter(YEAR > 1977, variable == "commercial landings"), 
       aes(x=YEAR, y=valueNAfill, fill=hydraFleets)) +
  geom_bar(stat = "identity") +
  facet_wrap(~modelName, scales = "free_y")


# see total aggregated values to hydraFleets
ggplot(catchIndexGearNAfill, aes(x=YEAR, y=valueNAfill, fill=variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~modelName, scales = "free_y")

