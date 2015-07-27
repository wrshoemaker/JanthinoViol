


rm(list=ls())
getwd()
setwd('~/github//JanthinoViol//')
getwd()
comp <- read.csv("data/Competition_07232015_WJB.csv", header = T)

library(ggplot2)
library(MASS)
wilcox.test(comp$BRatio, comp$ARatio, paired=TRUE) 

# Now for a plot, I got a little crazy
p<-ggplot(meltedComp, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE) + 
  geom_boxplot(width=0.1) + theme_minimal()
p

