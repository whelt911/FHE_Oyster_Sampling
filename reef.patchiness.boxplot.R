####Look at patchiness of reefs####

#Load packages
library("plyr")

#Set working directory
setwd("Z:/Will's Drive/R_wd/fhe/FHE_Oyster_Sampling")

#Load data
load("oyster.data")
load("quadrat.data")


#Figure out number of quadrats performed at each reef
quad.number <- aggregate(quadrat.data$Quadrat.Number ~ quadrat.data$Site.ID+quadrat.data$Date.Collected,
                         FUN = max)
colnames(quad.number) <- c('Site.ID','Date.Collected','Num.of.Quads')

oyster.data <- merge(oyster.data, quad.number, by = c('Site.ID','Date.Collected'))

#Find number of oysters per quadrat
oyster.count <- count(oyster.data, c('Site.ID','Date.Collected','Quadrat.Number'))

#Include 0's
quadrat.data <- merge(quadrat.data, oyster.count, by = c('Site.ID','Date.Collected','Quadrat.Number'),
                      all.x = TRUE)
quadrat.data$Total.Oyster.Count <- quadrat.data$Live.Oyster.Count+quadrat.data$Dead.Oyster.Count+
  quadrat.data$freq
quadrat.data[is.na(quadrat.data)] <- 0

oyster.count.data <- quadrat.data[,c(1,2,3,11)]

#Remove control & unseeded reefs
only.seeded <- subset(oyster.count.data, oyster.count.data$Site.ID !='Nin-1C')
only.seeded <- subset(only.seeded, only.seeded$Site.ID !='Nin-2C')
only.seeded <- subset(only.seeded, only.seeded$Site.ID !='Nin-3C')
only.seeded <- subset(only.seeded, only.seeded$Site.ID !='Nin-4C')
only.seeded <- subset(only.seeded, only.seeded$Site.ID !='Nin-1U')
only.seeded <- subset(only.seeded, only.seeded$Site.ID !='Nin-2U')
only.seeded <- subset(only.seeded, only.seeded$Site.ID !='Nin-3U')
only.seeded <- subset(only.seeded, only.seeded$Site.ID !='Nin-4U')
only.seeded <- droplevels(only.seeded)


#Create boxplot of oyster abundance
boxplot(Total.Oyster.Count ~ Site.ID, data = oyster.count.data,
        xlab = "Reef", ylab = "Oysters per 1/4 m^2", main = "Boxplots of Oyster Density on Reefs")

boxplot(Total.Oyster.Count ~ Site.ID, data = only.seeded,
        xlab = "Reef", ylab = "Oysters per 1/4 m^2", main = "Boxplots of Oyster Density on Reefs")