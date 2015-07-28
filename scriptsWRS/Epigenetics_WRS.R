rm(list=ls())
getwd()
setwd('~/github//JanthinoViol/')
getwd()

library(ggplot2)
library(lattice)
library(lsmeans)
library(multcompView)
library(plyr)

epi <- read.csv("./data/EpigeneticsViolacein_07272015_WJB.csv", header = T)
epi.melt <- melt(epi)
qqnorm(epi.melt$value)
wilcox.test(epi$Aza, epi$Control, paired=T) 

# So there isn't a significant difference and we can tell by just looking
# at the data there's something not right with it.

ggplot(epi.melt, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  geom_jitter()

# We  have a really wide variance, so let's see what happens if 
# we log-transform the data
epi$AzaLog <- log(epi$Aza, 10)
epi$ControlLog <- log(epi$Control, 10)

wilcox.test(epi$AzaLog, epi$ControlLog, paired=T) 
# Still not significant, but the graph looks better
meltepiLog <- melt(subset(epi, select = c(AzaLog,ControlLog)))

ggplot(meltepiLog, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  geom_jitter()