# Competition Colony Count Analysis
# Jared Brewer
# Created: July 21, 2015
# Modified: July 23, 2015

# The end goal here is to make a bar plot and perform an ANOVA.

comp <- read.csv("./Competition_07232015_WJB.csv", header = T)
attach(comp)

# Consider the ratio between the purple and the white instead of the raw numbers (since they probably don't mean anything anyway).

comp.aov <- aov(BRatio~ARatio)
summary(comp.aov)
TukeyHSD(comp.aov)

means <- c(mean(comp$BRatio, na.rm=T), mean(comp$ARatio, na.rm=T))
comp.std <- c(sd(comp$BRatio, na.rm=T), sd(comp$ARatio, na.rm=T)) 
comp.n <- c(sum(comp$BRatio, na.rm=T)/mean(comp$BRatio, na.rm=T), sum(comp$ARatio, na.rm=T)/mean(comp$ARatio, na.rm=T))
comp.se <- c(comp.std/sqrt(comp.n))

comp.bar <- barplot(means, names = c("Before", "After"), xlab="Time Point", ylab="CFU/mL (Ratio Purple:White)", ylim=c(0, 1), col=c("mediumorchid1", "mediumorchid4"), beside=TRUE)
abline(h=0)
segments(comp.bar, means+comp.se, comp.bar, means-comp.se) 
# Applies vertical marks for standard error
segments(comp.bar -0.05, means+comp.se, comp.bar +0.05, means+comp.se) 
# Makes horizontal tick on positive SE
# Numbers determine the width of the ticks
segments(comp.bar -0.05, means-comp.se, comp.bar +0.05, means-comp.se) 