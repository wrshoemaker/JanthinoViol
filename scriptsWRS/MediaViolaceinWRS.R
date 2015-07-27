rm(list=ls())
getwd()
setwd('~/github//JanthinoViol/data/')
getwd()

library(ggplot2)
library(lattice)
library(lsmeans)
library(multcompView)
library(plyr)

# The end goal here is to perform an ANOVA and make a bar plot.

media <- read.csv("./MediaViolacein_07212015_WJB.csv", header = T)

attach(media)
# Pairwise scatter plot
pairs(media, pch=20)

# omnibus test
#use anova(object) to test the omnibus hypothesis
#Are main or interaction effects present in the independent variables?
violaov <- anova(lm(media$Violacein ~ media$Media * media$Phenotype))
<<<<<<< HEAD
interaction.plot(media$Phenotype, media$Media, media$Violacein)
=======

interaction.plot(media$Violacein, media$Media, media$Media)
# This gave 26 warnings and wasn't able to produce a plot for me. -J
>>>>>>> upstream/master

# So there's a significant omnibus interaction for media, 
# but not for the interaction of media and phenotypes
# so we should focus on the main effects, in this case media

# We can visualize what we did above 
dotplot(media$Violacein ~ media$Media | media$Phenotype)
# This works fine. - J

# And we can use Tukey's test to see which media types are different
violTuk <- TukeyHSD(aov(Violacein ~ Media, data=media))
violTuk
# So, the comparison of LBYW-LBW, LBYW-LBY, and LBYW-LB yield significant results

# So the issue now is what to do when only the main effect is significant for the 
# Omnibus test, but not the inteaction? 

# In the meantime we can make a boxplot
# First, get our data into long form by merging the columns "phenotype"
# and "Media"
media$Variables <- do.call(paste, c(media[c("Media", "Phenotype")], sep = "")) 

ggplot(media, aes(x=Variables, y=Violacein, fill=Variables)) + geom_boxplot()
# This is really interesting, the violacein concentration does not really vary by phenotype
#

b2<-ggplot(media, aes(Variables, Violacein)) + 
  geom_jitter(aes(color=Variables)) +
  theme(legend.position = "none")