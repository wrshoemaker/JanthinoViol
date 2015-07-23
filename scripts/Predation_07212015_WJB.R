# Predation Colony Count Analysis
# Jared Brewer
# Created: July 21, 2015
# Modified: July 22, 2015

# The end goal here is to perform an ANOVA and make a bar plot. 

pred <- read.csv("./Predation_07232015_WJB.csv", header = T)
attach(pred)

pred.aov <- aov(PTT+WTT~PW, data=pred)
summary(pred.aov)
TukeyHSD(pred.aov)

pred.means <- c(mean(pred$PTT, na.rm=T), mean(pred$WTT, na.rm=T), mean(pred$PW, na.rm=T))
pred.std <- c(sd(pred$PTT, na.rm=T), sd(pred$PTT, na.rm=T), sd(pred$WTT, na.rm=T), sd(pred$WTT, na.rm=T), sd(pred$PW, na.rm=T), sd(pred$PW, na.rm=T)) 
pred.n <- c(sum(pred$PTT, na.rm=T)/mean(pred$PTT, na.rm=T), sum(pred$WTT, na.rm=T)/mean(pred$WTT, na.rm=T), sum(pred$PW, na.rm=T)/mean(pred$PW, na.rm=T))
pred.se <- c(pred.std/sqrt(abs(pred.n)))

pred.bar <- barplot(pred.means, names = c("PTT", "PW", "WTT"), xlab="Conditions", ylab="Violacein Units", ylim=c(0, 1), col=c("darkorchid4", "mediumorchid1", "white"))
abline(h=0)
segments(pred.bar, pred.means+pred.se, pred.bar, pred.means-pred.se) 
# Applies vertical marks for standard error
segments(pred.bar -0.05, pred.means+pred.se, pred.bar +0.05, pred.means+pred.se) 
# Makes horizontal tick on positive SE
# Numbers determine the width of the ticks
segments(pred.bar -0.05, pred.means-pred.se, pred.bar +0.05, pred.means-pred.se) 