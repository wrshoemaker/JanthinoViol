# Starvation Violacein Analysis
# Jared Brewer
# Created: July 21, 2015
# Modified: July 21, 2015

# The end goal here is to make a line plot and perform a regression analysis. 
# First, set your working directory, it doesn't necessarily matter where it is.
# Format is setwd ("directory")
# e.g. setwd("~/GitHub/JanthinoViol/data)

# Import the two data files that are being used here. In this case, it is the separate data from both one and ten day cultures.
# This could get messy.
tenvio <- read.csv("./StarvationViolacein_TenDay_07212015_WJB.csv", header = T)
onevio <- read.csv("./StarvationViolacein_OneDay_07212015_WJB.csv", header = T)

# This will plot the relevant points.
plot(tenvio$Day, tenvio$P, col='purple')
points(tenvio$Day, tenvio$W, col='black')
abline(lm(Day~P, data=tenvio, col='purple'))
abline(lm(Day~W, data=tenvio, col='black'))
# So, that doesn't look like anything worthwhile. I must wonder why.
# Could a bar plot be a better representation of the data? 

# Run a regression and look for differences, one at a time.
tenvioP.reg <- lm(Day~P, data=tenvio)
summary(tenvioP.reg)
# Non-signficant P-value here. I'm not sure what else could be done.
# Caveat: I'm not convinced that violacein is a linear function of cell density?

tenvioW.reg <- lm(Day~W, data=tenvio)
summary(tenvioW.reg)
# Same here. No signficant result. What now? Since they are not linear, is there a good way to normalize?
# Stuck.

plot(onevio$Day, onevio$P, col='purple')
points(onevio$Day, onevio$W, col='black')
abline(lm(Day~P, data=onevio, col='purple'))
abline(lm(Day~W, data=onevio, col='black'))

onevioP.reg <- lm(Day~P, data=onevio)
summary(onevioP.reg)

onevioW.reg <- lm(Day~W, data=onevio)
summary(onevioW.reg)

# These are WAY better than the ten day ones. I wonder why that is. 
# I'm at another impasse.