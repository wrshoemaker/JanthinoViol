# Predation Colony Count Analysis
# Jared Brewer
# Created: July 21, 2015
# Modified: July 22, 2015

# The end goal here is to perform an ANOVA and make a bar plot. 

pred <- read.csv("./Predation_07212015_WJB.csv", header = T)
attach(pred)

pred.aov <- aov(PA+WA~Name, data=pred)
summary(pred.aov)
TukeyHSD(pred.aov)

pred2 <- read.csv("./Predation_07223015_WJB.csv", header = T)

pred.means <- c(mean(pred2$PPTT, na.rm=T), mean(pred2$WPTT, na.rm=T), mean(pred2$PWTT, na.rm=T), mean(pred2$WWTT, na.rm=T), mean(pred2$PWP, na.rm=T), mean(pred2$PWW, na.rm=T))
pred.std <- c(sd(pred2$PPTT, na.rm=T), sd(pred2$WPTT, na.rm=T), sd(pred2$PWTT, na.rm=T), sd(pred2$WWTT, na.rm=T), sd(pred2$PWP, na.rm=T), sd(pred2$PWW, na.rm=T)) 
pred.n <- c(sum(pred2$PPTT, na.rm=T)/mean(pred2$PPTT, na.rm=T), sum(pred2$WPTT, na.rm=T)/mean(pred2$WPTT, na.rm=T), sum(pred2$PWTT, na.rm=T)/mean(pred2$PWTT, na.rm=T), sum(pred2$WWTT, na.rm=T)/mean(pred2$WWTT, na.rm=T), sum(pred2$PWP, na.rm=T)/mean(pred2$PWP, na.rm=T), sum(pred2$PWW, na.rm=T)/mean(pred2$PWW, na.rm=T))
pred.se <- c(pred.std/sqrt(abs(pred.n)))

pred.bar <- barplot(pred.means, names = c("PPTT", "WPTT", "PWTT", "WWTT", "PWP", "PWW"), xlab="Conditions", ylab="Violacein Units", ylim=c(0, 1.5e+8), col=c("darkorchid4", "white"))
abline(h=0)
segments(pred.bar, pred.means+pred.se, pred.bar, pred.means-pred.se) 
# Applies vertical marks for standard error
segments(pred.bar -0.05, pred.means+pred.se, pred.bar +0.05, pred.means+pred.se) 
# Makes horizontal tick on positive SE
# Numbers determine the width of the ticks
segments(pred.bar -0.05, pred.means-pred.se, pred.bar +0.05, pred.means-pred.se) 