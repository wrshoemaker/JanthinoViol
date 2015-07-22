# Starvation Culture Colony Count Analysis
# Jared Brewer
# Created: July 22, 2015
# Modified: July 22, 2015

# Run a regression and make a line plot.

tencol <- read.csv("StarvationColony_TenDay_07222015_WJB.csv", header = T)
onecol <- read.csv("StarvationColony_OneDay_07222015_WJB.csv", header = T)

plot(tencol$Day, tencol$Purple, col='purple')
points(tencol$Day, tencol$White, col='black')
abline(lm(Day~Purple, data=tencol, col='purple'))
abline(lm(Day~White, data=tencol, col='black'))

# Run a regression and look for differences, one at a time.
tencolP.reg <- lm(Day~Purple, data=tencol)
summary(tencolP.reg)
# Significance! 

tencolW.reg <- lm(Day~White, data=tencol)
summary(tencolW.reg)
# More significance! 

plot(onecol$Day, onecol$Purple, col='purple')
points(onecol$Day, onecol$White, col='black')
abline(lm(Day~Purple, data=onecol, col='purple'))
abline(lm(Day~White, data=onecol, col='black'))

onecolP.reg <- lm(Day~Purple, data=onecol)
summary(onecolP.reg)
# Not signficant.

onecolW.reg <- lm(Day~White, data=onecol)
summary(onecolW.reg)
# Significant. 
