# Starvation Culture Colony Count Analysis
# Jared Brewer
# Created: July 22, 2015
# Modified: July 22, 2015

# Run a regression and make a line plot.

tencol <- read.csv("StarvationColony_TenDay_07232015_WJB.csv", header = T)
onecol <- read.csv("StarvationColony_OneDay_07232015_WJB.csv", header = T)

# Look at relative abundance and graph separately.
# Also think about doing ratios instead of absolute numbers.

plot(tencol$Day, tencol$Ratio, col='purple')
# So these look a lot better, but I'm confused about how to best separate the white and purple ones. So close.
abline(lm(Day~Ratio, data=tencol))

# Run a regression and look for differences, one at a time.
tencol.reg <- lm(Day~Ratio, data=tencol)
summary(tencol.reg)
# Significance! 

plot(onecol$Day, onecol$Ratio, col='purple')
abline(lm(Day~Ratio, data=onecol))

onecol.reg <- lm(Day~Ratio, data=onecol)
summary(onecol.reg)
