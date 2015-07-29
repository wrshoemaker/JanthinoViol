rm(list=ls())
getwd()
setwd('~/github/JanthinoViol/data/')
getwd()

library(ggplot2)
library(lattice)
library(lsmeans)
library(multcompView)
library(plyr)

qs <- read.csv("QSViolacein_07212015_WJB.csv", header = T)
qs.melt <- melt(qs)

# so two treatments, we can do a Wilcoxon rank sum test, since the data is unpaired 
wilcox.test(value ~ variable, data=qs.melt) 

# Plot it
ggplot(qs.melt, aes(x=variable, y=value, fill=variable)) + geom_boxplot() + xlab("Experimental Groups") + ylab("Violacein Units")+ scale_fill_manual(values=c("darkorchid4", "white"), name="Treatment", labels=c("White without Supernatant", "White with Supernatant"))  + geom_blank() 
