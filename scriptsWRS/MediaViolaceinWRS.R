rm(list=ls())
getwd()
setwd('~/github//JanthinoViol//')
getwd()

library(ggplot2)
library(lattice)
library(lsmeans)

# The end goal here is to perform an ANOVA and make a bar plot.

media <- read.csv("data/MediaViolacein_07212015_WJB.csv", header = T)

# omnibus test
#use anova(object) to test the omnibus hypothesis
#Are main or interaction effects present in the independent variables?
violaov <- anova(lm(media$Violacein ~ media$Media * media$Phenotype))

interaction.plot(media$Violacein, media$Media, media$Media)

# So there's a significant omnibus interaction for media, 
# but not for the interaction of media and phenotypes
# so we should focus on the main effects, in this case media

# We can visualize what we did above 
dotplot(media$Violacein ~ media$Media | media$Phenotype)

# And we can use Tukey's test to see which media types are different

ggplot(media, aes(x = Media, y = Violacein)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Medium") +
  ylab("Violacein Units")