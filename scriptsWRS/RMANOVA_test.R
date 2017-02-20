rm(list=ls())
getwd()
setwd('~/github/JanthinoViol/data')
getwd()

library(xlsx)
library(nlme)

lme(lim*time, random = ~1| cID, correlation = corAR1(form= ~1| cID))

dataRepeatedMeasures <- read.xlsx("RMANOVA_data.xlsx", 4)

model <- lme(Viol ~ Treatment * Timepoint, random = ~1|Subject, 
             correlation = corAR1(form= ~1|Subject), data = dataRepeatedMeasures)
