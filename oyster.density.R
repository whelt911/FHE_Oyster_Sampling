####Oyster Density####

#Load packages
library("plyr")
library("sciplot")
library("ggplot2")

#Set working directory
setwd("H:/Will's Drive/R_wd/fhe/FHE_Oyster_Sampling")

#Load data
load("oyster.data")
load("quadrat.data")

####Live & Dead Oyster Density####
#Estimated Total seeded density (live and dead) <--should probably only be useful for 1st round of monitoring
##This is from quadrat sampling, not estimation before seeding

#Quantify number of live and dead oysters measured per quadrat

count.measured <- count(oyster.data, c('Site.ID','Date.Collected','Quadrat.Number'))

#Provide table with all the live and dead oysters counted per quadrat
count.data <- oyster.data[,c(1,2,3,13,14,15)]
count.data <- unique(count.data)
count.data$Total.Count <- count.data$Live.Oyster.Count + count.data$Dead.Oyster.Count
count.data <- count.data[,c(1,2,3,7)]


#Merge measured counts and subsampled counts
oyster.count <- merge(count.measured,count.data, 
                           by = c('Site.ID','Date.Collected','Quadrat.Number'))
oyster.count$Num.Oysters <- oyster.count$freq + oyster.count$Total.Count
oyster.count <- oyster.count[,c(-4,-5)]

#Merge counts with quadrat sampled data (to include 0 counts)
merged.data <- merge(quadrat.data, oyster.count, by = c('Site.ID','Date.Collected','Quadrat.Number'), 
                     all.x = TRUE)
merged.data <- merged.data[,-c(7,8,9)]
merged.data[is.na(merged.data)] <- 0 #replace NA's with 0's

#Mean & SE of Oysters by site
oyster.mean <- aggregate(merged.data$Num.Oysters ~ 
                          merged.data$Site.ID + merged.data$Date.Collected,
                        FUN = mean)
colnames(oyster.mean) <- c('Site.ID','Date.Collected','Mean')
oyster.se <- aggregate(merged.data$Num.Oysters ~ 
                         merged.data$Site.ID + merged.data$Date.Collected,
                       FUN = se)
colnames(oyster.se) <- c('Site.ID','Date.Collected','SE')

oyster.mean.se <- merge(oyster.mean, oyster.se, by = c('Site.ID','Date.Collected'))

####Barplot of Live & Dead Oysters per 1/4m2####
plot.oyster.mean.density <- 
  ggplot(oyster.mean.se, aes(x=Site.ID, y=Mean)) + 
  geom_bar(position=position_dodge(), stat="identity", colour = "grey") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("Mean Oyster Density per 1/4m2") + 
  xlab("Reef ID") + 
  ggtitle("Mean Oyster Density on Reefs (Live & Dead)")+
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 24),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 24),
        plot.title = element_text(size = 32))
plot.oyster.mean.density

####Live Oyster Density####

#Subset only live oysters
live.oyster.data <- subset(oyster.data, oyster.data$Alive.Dead == "8")

#Quantify number of liveoysters measured per quadrat

live.count.measured <- count(live.oyster.data, c('Site.ID','Date.Collected','Quadrat.Number'))

#Provide table with all the live and dead oysters counted per quadrat
live.count.data <- live.oyster.data[,c(1,2,3,13,14,15)]
live.count.data <- unique(live.count.data)
live.count.data <- live.count.data[,-6]

#Merge measured counts and subsampled counts
live.oyster.count <- merge(live.count.measured, live.count.data, 
                      by = c('Site.ID','Date.Collected','Quadrat.Number'))
live.oyster.count$Num.Oysters <- live.oyster.count$freq + live.oyster.count$Live.Oyster.Count
live.oyster.count <- live.oyster.count[,c(-4,-5,-6)]

#Merge counts with quadrat sampled data (to include 0 counts)
live.merged.data <- merge(quadrat.data, live.oyster.count, by = c('Site.ID','Date.Collected','Quadrat.Number'), 
                     all.x = TRUE)
live.merged.data <- live.merged.data[,-c(7,8,9)]
live.merged.data[is.na(live.merged.data)] <- 0 #replace NA's with 0's

#Mean & SE of Live Oysters by site
live.oyster.mean <- aggregate(live.merged.data$Num.Oysters ~ 
                           live.merged.data$Site.ID + live.merged.data$Date.Collected,
                         FUN = mean)
colnames(live.oyster.mean) <- c('Site.ID','Date.Collected','Mean')
live.oyster.se <- aggregate(live.merged.data$Num.Oysters ~ 
                         live.merged.data$Site.ID + live.merged.data$Date.Collected,
                       FUN = se)
colnames(live.oyster.se) <- c('Site.ID','Date.Collected','SE')

live.oyster.mean.se <- merge(live.oyster.mean, live.oyster.se, by = c('Site.ID','Date.Collected'))

####Barplot of Live Oysters per 1/4m2####
plot.live.oyster.mean.density <- 
  ggplot(live.oyster.mean.se, aes(x=Site.ID, y=Mean)) + 
  geom_bar(position=position_dodge(), stat="identity", colour = "grey") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("Mean Live Oyster Density per 1/4m2") + 
  xlab("Reef ID") + 
  ggtitle("Mean Live Oyster Density on Reefs")+
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 24),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 24),
        plot.title = element_text(size = 32))
plot.live.oyster.mean.density

####Dead Oyster Density####
#Subset only dead oysters
dead.oyster.data <- subset(oyster.data, oyster.data$Alive.Dead == "9")

#Quantify number of deadoysters measured per quadrat

dead.count.measured <- count(dead.oyster.data, c('Site.ID','Date.Collected','Quadrat.Number'))

#Provide table with all the dead and dead oysters counted per quadrat
dead.count.data <- dead.oyster.data[,c(1,2,3,13,14,15)]
dead.count.data <- unique(dead.count.data)
dead.count.data <- dead.count.data[,-5]

#Merge measured counts and subsampled counts
dead.oyster.count <- merge(dead.count.measured, dead.count.data, 
                           by = c('Site.ID','Date.Collected','Quadrat.Number'))
dead.oyster.count$Num.Oysters <- dead.oyster.count$freq + dead.oyster.count$Dead.Oyster.Count
dead.oyster.count <- dead.oyster.count[,c(-4,-5,-6)]

#Merge counts with quadrat sampled data (to include 0 counts)
dead.merged.data <- merge(quadrat.data, dead.oyster.count, by = c('Site.ID','Date.Collected','Quadrat.Number'), 
                          all.x = TRUE)
dead.merged.data <- dead.merged.data[,-c(7,8,9)]
dead.merged.data[is.na(dead.merged.data)] <- 0 #replace NA's with 0's

#Mean & SE of dead Oysters by site
dead.oyster.mean <- aggregate(dead.merged.data$Num.Oysters ~ 
                                dead.merged.data$Site.ID + dead.merged.data$Date.Collected,
                              FUN = mean)
colnames(dead.oyster.mean) <- c('Site.ID','Date.Collected','Mean')
dead.oyster.se <- aggregate(dead.merged.data$Num.Oysters ~ 
                              dead.merged.data$Site.ID + dead.merged.data$Date.Collected,
                            FUN = se)
colnames(dead.oyster.se) <- c('Site.ID','Date.Collected','SE')

dead.oyster.mean.se <- merge(dead.oyster.mean, dead.oyster.se, by = c('Site.ID','Date.Collected'))

#Mean & SE of Oysters by site
live.oyster.mean.se$Alive.Dead <- "Alive"
dead.oyster.mean.se$Alive.Dead <- "Dead"

LD.oyster.mean.se <- rbind(live.oyster.mean.se,dead.oyster.mean.se)

####Stacked Barplot of live and dead oysters per 1/4m2####
stacked.barplot.oyster.mean.density <-
  ggplot(LD.oyster.mean.se, aes(x=Site.ID, y=Mean, fill=Alive.Dead)) +
  geom_bar(position="stack", stat = "identity") +
  xlab('Reef ID') +
  ylab('Mean Oyster Density per 1/4m2') +
  ggtitle('Mean Oyster Density on Reefs')+
  guides(fill=guide_legend(title="Oyster Status"))
stacked.barplot.oyster.mean.density

####Boxplot of Total oyster density per reef####

