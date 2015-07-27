rm(list=ls())
getwd()
setwd('~/github//JanthinoViol/data/')
getwd()
comp <- read.csv("./Competition_07232015_WJB.csv", header = T)

library(ggplot2)
library(MASS)
wilcox.test(comp$BRatio, comp$ARatio, paired=TRUE) 
# This result is not a significant different, but it is close. - J

# Now for a plot, I got a little crazy
p <- ggplot(meltedComp, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE) + 
  geom_boxplot(width=0.1) + theme_minimal()
p

# It gives me the error that "object meltedComp not found". I don't know what that means since i've never used ggplot before. -J
