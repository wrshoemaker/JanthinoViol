# Media Violacein Analysis
# Jared Brewer
# Created: July 21, 2015
# Modified: July 21, 2015

# The end goal here is to perform an ANOVA and make a bar plot. 

media <- read.csv("./MediaViolacein_07222015_WJB.csv", header = T)
LB <- read.csv("./LBViolacein_07222015_WJB.csv", header = T)
LBW <- read.csv("./LBWViolacein_07222015_WJB.csv", header = T)
LBY <- read.csv("./LBYViolacein_07222015_WJB.csv", header = T)
LBYW <- read.csv("./LBYWViolacein_07222015_WJB.csv", header = T)

# I have these data reformatted in a variety of ways, but I'm still not getting anywhere with actually getting it analyzed. Help?

white.aov <- aov(WLB~WLBW*WLBY*WLBYW, data=media)
summary(white.aov)

purple.aov <- aov(PLB~PLBW*PLBY*PLBYW, data=media)
summary(purple.aov)