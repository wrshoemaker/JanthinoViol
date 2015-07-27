rm(list=ls())
getwd()
setwd('~/github//JanthinoViol//')
getwd()

library(ggplot2)
library(lattice)
library(lsmeans)
library(multcompView)
library(plyr)

qs <- read.csv("data/QSViolacein_07212015_WJB.csv", header = T)
qs.melt <- melt(qs)

# so two treatments, we can do a Wilcoxon rank sum test, since the data is unpaired 
wilcox.test(value ~ variable, data=qs.melt) 

# Plot it
ggplot(qs.melt, aes(x=variable, y=value, fill=variable)) + geom_boxplot()

# Later, fix plots, make them nicer. 
