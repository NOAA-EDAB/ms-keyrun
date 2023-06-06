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


ggplot(catchIndex, aes(x=YEAR, y=value, fill=variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Name, scales = "free_y")
