rm(list=ls())
getwd()
setwd('~/github/JanthinoViol/data/')
getwd()
library(ggplot2)
library(MASS)
library(reshape2)

compCFU <- read.csv("./Competition_07212015_WJB.csv", header = T)
compRA <- read.csv("./Competition_07232015_WJB.csv", header = T)

# First we'll look at the total CFU difference before and after compeition,
# Then we'll look at the relative abundances (RA)

# First, add a column summing purple and white CFUs
compCFU$BeforeTotal <- as.numeric(compCFU$PB) + as.numeric(compCFU$WB)
compCFU$AfterTotal <- as.numeric(compCFU$PA) + as.numeric(compCFU$WA)
# Then we'll log-transform
compCFU$BeforeTotalLog <- log(compCFU$BeforeTotal, 10)
compCFU$AfterTotalLog <- log(compCFU$AfterTotal, 10)
compCFU$PBlog <- log(compCFU$PB, 10)
compCFU$WBlog <- log(compCFU$WB, 10)
compCFU$PAlog <- log(compCFU$PA, 10)
compCFU$WAlog <- log(compCFU$WA, 10)

# Run a Wilcoxon signed-rank test
wilcox.test(compCFU$BeforeTotalLog, compCFU$AfterTotalLog, paired=TRUE) 
# Not significant 
# Plot it
compCFUsubset <- subset(compCFU, select = c(BeforeTotalLog,AfterTotalLog, PBlog, WBlog, PAlog, WAlog))
meltCFU <- melt(compCFUsubset)

ggplot(meltCFU, aes(x=variable, y=value, fill=variable)) + geom_boxplot()
# Not much of a trend

# Let's look at the RA data
meltRA <- melt(compRA)
ggplot(meltRA, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  geom_jitter()

# Not too bad so far, after there's more variation, a larger median
# Let's see how it handles a Wilcoxon signed-rank test
wilcox.test(compRA$BRatio, compRA$ARatio, paired=TRUE) 
# Not significant, but under the assumptions of the test
# But, this is still a large increase. 
