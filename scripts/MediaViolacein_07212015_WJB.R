# Media Violacein Analysis
# Jared Brewer
# Created: July 21, 2015
# Modified: July 21, 2015

# The end goal here is to perform an ANOVA and make a bar plot. 

media <- read.csv("./MediaViolacein_07212015_WJB.csv", header = T)

media.aov <- aov(Violacein~Media+Phenotype, data=media)
summary(media.aov)
TukeyHSD(media.aov)

# Graphing the data may require a little reformatting, but these two-level ANOVAs are great! 

media2 <- read.csv("./MediaViolacein_07222015_WJB.csv", header = T)

media.means <- c(mean(media2$PLB, na.rm=T), mean(media2$WLB, na.rm=T), mean(media2$PLBW, na.rm=T), mean(media2$WLBW, na.rm=T), mean(media2$PLBY, na.rm=T), mean(media2$WLBY, na.rm=T), mean(media2$PLBYW, na.rm=T), mean(media2$WLBYW, na.rm=T))
media.std <- c(sd(media2$PLB, na.rm=T), sd(media2$WLB, na.rm=T), sd(media2$PLBW, na.rm=T), sd(media2$WLBW, na.rm=T), sd(media2$PLBY, na.rm=T), sd(media2$WLBY, na.rm=T), sd(media2$PLBYW, na.rm=T), sd(media2$WLBYW, na.rm=T)) 
media.n <- c(sum(media2$PLB, na.rm=T)/mean(media2$PLB, na.rm=T), sum(media2$WLB, na.rm=T)/mean(media2$WLB, na.rm=T), sum(media2$PLBW, na.rm=T)/mean(media2$PLBW, na.rm=T), sum(media2$WLBW, na.rm=T)/mean(media2$WLBW, na.rm=T), sum(media2$PLBY, na.rm=T)/mean(media2$PLBY, na.rm=T), sum(media2$WLBY, na.rm=T)/mean(media2$WLBY, na.rm=T), sum(media2$PLBYW, na.rm=T)/mean(media2$PLBYW, na.rm=T), sum(media2$WLBYW, na.rm=T)/mean(media2$WLBYW, na.rm=T))
media.se <- c(media.std/sqrt(abs(media.n)))

media.bar <- barplot(media.means, names = c("LB", "LB", "LB-Tryptophan", "LB-Tryptophan", "LB-Glycerol", "LB-Glycerol", "LB-Both", "LB-Both"), xlab="Media Type", ylab="Violacein Units", ylim=c(0, 16), col=c("darkorchid4", "white"))
abline(h=0)
segments(media.bar, media.means+media.se, media.bar, media.means-media.se) 
# Applies vertical marks for standard error
segments(media.bar -0.05, media.means+media.se, media.bar +0.05, media.means+media.se) 
# Makes horizontal tick on positive SE
# Numbers determine the width of the ticks
segments(media.bar -0.05, media.means-media.se, media.bar +0.05, media.means-media.se) 