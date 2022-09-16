rm(list=ls())

# visualize diet comps
# December 2016

library(ggplot2)
library(tidyr)
library(dplyr)

dogdiet   <- read.csv("dogfishdietcomp.csv")
GBcoddiet <- read.csv("GBcoddietcomp.csv")
GOMcoddiet <- read.csv("GOMcoddietcomp.csv")

names(dogdiet)[1]<-"Year"
names(GBcoddiet)[1]<-"Year" 
names(GOMcoddiet)[1]<-"Year"

dogdiet <- cbind(dogdiet, Pred=rep("dogfish", dim(dogdiet)[1]))
GBcoddiet <- cbind(GBcoddiet, Pred=rep("GBcod", dim(GBcoddiet)[1]))
GOMcoddiet <- cbind(GOMcoddiet, Pred=rep("GOMcod", dim(GOMcoddiet)[1]))

dogdiet <- gather(dogdiet, Prey, Percent, -Year, -Pred)
GBcoddiet <- gather(GBcoddiet, Prey, Percent, -Year, -Pred)
GOMcoddiet <- gather(GOMcoddiet, Prey, Percent, -Year, -Pred)

dogcoddiet <- bind_rows(dogdiet, GBcoddiet, GOMcoddiet)

dogcoddiet$fillwhite <- with(dogcoddiet, ifelse(Prey=="CLUHAR", 1,0))

dogcoddiet$fillwhite <- with(dogcoddiet, ifelse(Prey=="BRETYR", 1,0))

compplot <- ggplot(dogcoddiet, aes(Year, Percent, fill=Prey)) + 
  #geom_bar(stat = "identity", aes(fill=fillwhite)) 
  geom_bar(stat = "identity") 
  
compplot + facet_wrap("Pred", nrow=3) + guides(fill=FALSE)
# +theme(legend.position="none")

compplot2 <- ggplot(dogcoddiet, aes(Year, Percent, colour=Prey)) + 
  geom_bar(stat = "identity", aes(fill=fillwhite)) 
  #geom_bar(stat = "identity") 

compplot2 + facet_wrap("Pred", nrow=3) + theme(legend.position="none")
  #guides(colour=FALSE)
ggsave("compplot2.png", scale=2)

dogcompplot <- ggplot(dogdiet, aes(Year, Percent, fill=Prey)) + 
  geom_bar(stat = "identity") 

dogcompplot + theme(legend.position="none")
