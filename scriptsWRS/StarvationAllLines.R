rm(list=ls())
getwd()
setwd('~/github/JanthinoViol/data')
getwd()

library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(plyr)
library(Rmisc)

Viol1day <- read.csv("StarvationViolacein_OneDay_07212015_WJB.csv", header = T)
Viol10day <- read.csv("StarvationViolacein_TenDay_07212015_WJB.csv", header = T) 
ViolInf <- read.csv("StarvationViolacein_Infinite_07272015_WJB.csv", header = T) 

# So first we take subsets of the data that we're interested in
Viol1Sub <- Viol1day[13:15,]
Viol10Sub <- Viol10day[13:15,]

# Then we combine the data
ViolMerge <- rbind(Viol1Sub, Viol10Sub, ViolInf)
Names <- c("1","1","1","10","10", "10", "Inf", "Inf", "Inf")
ViolMerge$Day <- NULL
ViolMerge$labels <- Names
# Now melt and merge
ViolMelt <- melt(ViolMerge)
ViolMelt$Variables <- do.call(paste, c(ViolMelt[c("variable", "labels")], sep = "")) 

# Now plot
ggplot(ViolMelt, aes(x=Variables, y=value, fill=variable)) + 
  geom_boxplot()

# Let's start an ANOVA

