# Here we are going to analyze colony count and 
# violacein extraction data for purple and white strains 
# Across sampling points

#Either work on time series or repeated measures. 

rm(list=ls())
getwd()
setwd('~/github/JanthinoViol/data')
getwd()

library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(plyr)
library(Rmisc)
library(xlsx)
library(lme4)

# load data
CFU1count <-  read.csv("StarvationColony_OneDay_07222015_WJB.csv", header = T)
CFU10count <- read.csv("StarvationColony_TenDay_07222015_WJB.csv", header = T)

CFU1dayRA <- read.csv("StarvationColony_OneDay_07232015_WJB.csv", header = T)
CFU10dayRA <- read.csv("StarvationColony_TenDay_07232015_WJB.csv", header = T)

Viol1day <- read.csv("StarvationViolacein_OneDay_07212015_WJB.csv", header = T)
Viol10day <- read.csv("StarvationViolacein_TenDay_07212015_WJB.csv", header = T) 

# Sum the purple and white CFU counts for 1 and 10 day lines
CFU1count$Total <- as.numeric(CFU1count$Purple) + as.numeric(CFU1count$White)
CFU10count$Total <- as.numeric(CFU10count$Purple) + as.numeric(CFU10count$White)

# These data sets are pretty much the same, 10 day and 1 day lines are just different treatments
# So we can just add a column designating whether a row is from a 1 or 10 day line, and merge them
meCFU10count$Transfer <- rep(10,nrow(CFU10count))

# Now merge on the columns "Treatment"
CFUcountMerge <- rbind(CFU1count, CFU10count)
CFUcountMergesubset <- subset(CFUcountMerge, select = -c(Purple,White) )
CFUcountMergesubset$Variables <- do.call(paste, c(CFUcountMergesubset[c("Name", "Transfer")], sep = "")) 

# Let's also do a log-transform

CFUcountMergesubset$Total <- log(CFUcountMergesubset$Total, 10)

# Now let's do the same thing for everything else
CFU1dayRA$Transfer <- rep(1,nrow(CFU1dayRA)) 
CFU10dayRA$Transfer <- rep(10,nrow(CFU10dayRA)) 
CFURAmerge <- rbind(CFU1dayRA, CFU10dayRA)
CFURAmerge$Variables <- do.call(paste, c(CFURAmerge[c("Name", "Transfer")], sep = "")) 

# We need to first reshape the violacein datasets before we can merge them
Viol1melt <- melt(Viol1day, id.vars = "Day")
Viol10melt <- melt(Viol10day, id.vars = "Day")

Viol1melt$Transfer <- rep(1,nrow(Viol1melt)) 
Viol10melt$Transfer <- rep(10,nrow(Viol10melt)) 
ViolMerge <- rbind(Viol1melt, Viol10melt)
ViolMerge$Variables <- do.call(paste, c(ViolMerge[c("variable", "Transfer")], sep = "")) 

# Mean and standard error
CFUcountSE <- summarySE(CFUcountMergesubset, measurevar="Total", groupvars=c("Day", "Variables"),
                         conf.interval = 0.95, .drop = TRUE)

CFUraSE <- summarySE(CFURAmerge, measurevar="Ratio", groupvars=c("Day", "Variables"),
                    conf.interval = 0.95, .drop = TRUE)

# Now get the mean and standard error fot violacein extraction 
# for each phenotype
ViolSE <- summarySE(ViolMerge, measurevar="value", groupvars=c("Day", "Variables"),
          conf.interval = 0.95, .drop = TRUE)

# Now let's plot violacein concentration and CFU counts through time
# First up, plot CFU concentration 
ggplot(CFUcountSE, aes(x=Day, y=Total, colour=Variables)) + 
  geom_errorbar(aes(ymin=Total-se, ymax=Total+se), width=.1) +
  geom_line() +
  geom_point() + xlab("Day") + ylab("CFUs/mL")

# Now the relative abundance 
ggplot(CFUraSE, aes(x=Day, y=Ratio, colour=Variables)) + 
  geom_errorbar(aes(ymin=Ratio-se, ymax=Ratio+se), width=.1) +
  geom_line() +
  geom_point() + xlab("Day") + ylab("Ratio (CFU Purple:White)")

# Violacein concentration 
ggplot(ViolSE, aes(x=Day, y=value, colour=Variables)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
  geom_line() +
  geom_point() + xlab("Day") + ylab("Violacein Units")


###
###
### Here we are starting our repeated measurs ANOVA

# First, reformat the data
require(plyr)
# subset violacein so our data frames are of the same size
subsetViol <- subset(ViolMerge, ViolMerge[ , 1] < 52)  
# So the data is still unequal in length. I'll need to revisit this/ how to work on this


CFUcountMergesubset <- data.frame(CFUcountMergesubset)
CFURAmerge <- data.frame(CFURAmerge)
ViolMerge <- data.frame(ViolMerge)

CFUcountMergesubset$rn <- rownames(CFUcountMergesubset)
CFURAmerge$rn <- rownames(CFURAmerge)
ViolMerge$rn <- rownames(ViolMerge)

df <- join_all(list(CFUcountMergesubset,CFURAmerge,ViolMerge), by = 'rn', type = 'full')
test <- merge(CFUcountMergesubset,ViolMerge, by="row.names", all=TRUE)



# I'm importing a datafile I made up quickly to get in the proper format
dataRepeatedMeasures <- read.xlsx("RMANOVA_data.xlsx", 4)


m1 <- lmer(Viol ~ Timepoint + (1|Subject), data=dataRepeatedMeasures)
m2 <- lmer (Viol ~ Timepoint + (Timepoint|Subject), data=dataRepeatedMeasures)
m3 <- lmer (Viol ~ Timepoint + Treatment + (Timepoint|Subject), data=dataRepeatedMeasures)
m4 <- lmer (Viol ~ Timepoint + Treatment + Timepoint*Treatment + (Timepoint|Subject), data=dataRepeatedMeasures)

library(nlme)
# Try with AOV function
analysis <- lme( Viol ~ Timepoint*  Treatment + Error( Subject / Timepoint), 
                 data = dataRepeatedMeasures )

lNull <- lme(Viol ~  Timepoint * Treatment,data=dataRepeatedMeasures, random= ~ 1 | Subject,
             method=’REML’)