####Reef Height####

#Load packages
library("plyr")
library("sciplot")
library("ggplot2")

#Set working directory
setwd("Z:/Will's Drive/R_wd/fhe/FHE_Oyster_Sampling")

#Load data
load("oyster.data")
load("quadrat.data")

#Remove controls
rm.controls <- subset(quadrat.data, quadrat.data$Site.ID != 'Nin-1C')
rm.controls <- subset(rm.controls, rm.controls$Site.ID != 'Nin-2C')
rm.controls <- subset(rm.controls, rm.controls$Site.ID != 'Nin-3C')
rm.controls <- subset(rm.controls, rm.controls$Site.ID != 'Nin-4C')

rm.controls <- droplevels(rm.controls)
boxplot(rm.controls$Relief.cm ~ rm.controls$Site.ID, xlab = "Reef ID", ylab = 'Reef Relief (mm)',
        main = 'Boxplot of Reef Relief by Site')
