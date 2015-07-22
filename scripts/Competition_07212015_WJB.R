# Competition Colony Count Analysis
# Jared Brewer
# Created: July 21, 2015
# Modified: July 22, 2015

# The end goal here is to make a bar plot and perform an ANOVA.

comp <- read.csv("./Competition_07212015_WJB.csv", header = T)
attach(comp)

# Consider the ratio between the purple and the white instead of the raw numbers (since they probably don't mean anything anyway).

comp.aov <- aov(PB*WB~PA*WA)
summary(comp.aov)

means <- c(mean(comp$PB, na.rm=T), mean(comp$PA, na.rm=T), mean(comp$WB, na.rm=T), mean(comp$WA, na.rm=T))
comp.std <- c(sd(comp$PB, na.rm=T), sd(comp$PA, na.rm=T), sd(comp$WB, na.rm=T), sd(comp$WA, na.rm=T)) 
comp.n <- c(sum(comp$PB, na.rm=T)/mean(comp$PA, na.rm=T), sum(comp$PB, na.rm=T)/mean(comp$PA, na.rm=T), sum(comp$WB, na.rm=T)/mean(comp$WA, na.rm=T), sum(comp$WB, na.rm=T)/mean(comp$WA, na.rm=T))
comp.se <- c(comp.std/sqrt(comp.n))

comp.bar <- barplot(means, log="y", names = c("PB", "PA", "WB", "WA"), xlab="Strain", ylab="CFU/mL", ylim=c(1e+9, 6e+9), col=c("steelblue1", "firebrick1"), beside=TRUE)
abline(h=0)
segments(comp.bar, means+comp.se, comp.bar, means-comp.se) 
# Applies vertical marks for standard error
segments(comp.bar -0.05, means+comp.se, comp.bar +0.05, means+comp.se) 
# Makes horizontal tick on positive SE
# Numbers determine the width of the ticks
segments(comp.bar -0.05, means-comp.se, comp.bar +0.05, means-comp.se) 

# Latent issues include colors (easily corrected) and grouping them properly (not sure about this one).