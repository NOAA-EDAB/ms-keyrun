# plot weight at age time series facet wrapped by species
wageplot <- function(dat, truedat=NULL){
  ggplot(dat, aes(year, value)) +
    geom_line(aes(colour = factor(age))) +
    ggthemes::theme_tufte() +
    theme(legend.position = "bottom") +
    xlab("model year") +
    ylab("average individual weight (g)") +
    labs(subtitle = paste0(scenario.name)) +
    facet_wrap(c("Name"), scales="free_y")
}
