# Predation Colony Count Analysis
# Jared Brewer
# Created: July 21, 2015
# Modified: July 22, 2015

# The end goal here is to perform an ANOVA and make a bar plot. 

pred <- read.csv("./Predation_07222015_WJB.csv", header = T)
attach(pred)

# I think I have a data formatting issue that I don't understand. 
# I'm getting nowhere.

ppred.aov <- aov(PPTT*WPTT~PWP*PWW, data=pred)
summary(ppred.aov)

wpred.aov <- aov(PWTT*WWTT~PWP*PWW, data=pred)
summary(wpred.aov)