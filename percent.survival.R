####Oyster % Survival####

#Load packages
library("plyr")
library("sciplot")
library("ggplot2")

#Set working directory

#Load data
load("oyster.data")
load("quadrat.data")



#Quantify number of live and dead oysters measured per quadrat
live.oyster <- subset(oyster.data, oyster.data$Alive.Dead == "8")   #all live oysters measured
dead.oyster <- subset(oyster.data, oyster.data$Alive.Dead == "9")   #all dead oysters measured

live.oyster <- count(live.oyster, c('Site.ID','Date.Collected','Quadrat.Number'))
dead.oyster <- count(dead.oyster, c('Site.ID','Date.Collected','Quadrat.Number'))

#Provide table with all the live and dead oysters counted per quadrat
count.data <- oyster.data[,c(1,2,3,13,14,15)]
count.data <- unique(count.data)

live.count.data <- count.data[,-6]
dead.count.data <- count.data[,-5]

#Merge measured counts and subsampled counts
live.oyster.count <- merge(live.oyster,live.count.data, 
                           by = c('Site.ID','Date.Collected','Quadrat.Number'))
dead.oyster.count <- merge(dead.oyster,dead.count.data, 
                           by = c('Site.ID','Date.Collected','Quadrat.Number'))

#add 2 counts together
live.oyster.count$Total <- live.oyster.count$freq+live.oyster.count$Live.Oyster.Count
dead.oyster.count$Total <- dead.oyster.count$freq+dead.oyster.count$Dead.Oyster.Count

#Rename and merge live/dead counts
live.oyster.count <- live.oyster.count[,-c(4,5,6)]
dead.oyster.count <- dead.oyster.count[,-c(4,5,6)]

colnames(live.oyster.count) <- c('Site.ID','Date.Collected','Quadrat.Number','Total.Live')
colnames(dead.oyster.count) <- c('Site.ID','Date.Collected','Quadrat.Number','Total.Dead')

final.oyster.count <- merge(live.oyster.count, dead.oyster.count, 
                            by = c('Site.ID','Date.Collected','Quadrat.Number'))
final.oyster.count$Total.All <- final.oyster.count$Total.Live+final.oyster.count$Total.Dead
final.oyster.count$Pct.Alive <- final.oyster.count$Total.Live/final.oyster.count$Total.All*100

#Mean and std. error survival
mean.oyster.survival <- aggregate(final.oyster.count$Pct.Alive ~ final.oyster.count$Site.ID+
                                    final.oyster.count$Date.Collected, FUN = mean)
std.error.oyster.survival <- aggregate(final.oyster.count$Pct.Alive ~ final.oyster.count$Site.ID+
                                         final.oyster.count$Date.Collected, FUN = se)
##rename columns
colnames(mean.oyster.survival) <- c('Site.ID','Date.Collected','Mean.Pct.Alive')
colnames(std.error.oyster.survival) <- c('Site.ID','Date.Collected','Standard.Error')

#merge
oyster.survival <- merge(mean.oyster.survival, std.error.oyster.survival, by = c('Site.ID','Date.Collected'))

#Plot
plot.oyster.survival <- ggplot(oyster.survival, aes(x=Site.ID, y=Mean.Pct.Alive)) + 
  geom_bar(position=position_dodge(), stat="identity", colour = "black") +
  geom_errorbar(aes(ymin=Mean.Pct.Alive-Standard.Error, ymax=Mean.Pct.Alive+Standard.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("Percent Survival of Oysters") + 
  xlab("Reef ID") + 
  ggtitle("Survival of Remote-set Oysters on Reef")+
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 24),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 24),
        plot.title = element_text(size = 32))
plot.oyster.survival
plot.oyster.survival +
  coord_cartesian(ylim=c(90,100))


####Spring 2016 Pct. Survival####

#Subset only oysters sampled in Spring 2016
spring2016.oyster.data <- subset(oyster.data, oyster.data$Date.Collected > "2016-05-01" )
spring2016.oyster.data <- subset(oyster.data, oyster.data$Date.Collected < "2016-05-30" )


#Quantify number of live and dead oysters measured per quadrat
live.oyster <- subset(oyster.data, oyster.data$Alive.Dead == "8")   #all live oysters measured
dead.oyster <- subset(oyster.data, oyster.data$Alive.Dead == "9")   #all dead oysters measured





####Fall 2016 Pct. Survival####
fall2016.oyster.data <- subset(oyster.data, oyster.data$Date.Collected > "2016-08-01")
fall2016.oyster.data <- subset(oyster.data, oyster.data$Date.Collected < "2016-12-31")

#Quantify number of live and dead oysters measured per quadrat
fall2016.live.oyster <- subset(fall2016.oyster.data, fall2016.oyster.data$Alive.Dead == "8")   #all live oysters measured
fall2016.dead.oyster <- subset(fall2016.oyster.data, fall2016.oyster.data$Alive.Dead == "9")   #all dead oysters measured

fall2016.live.oyster <- count(fall2016.live.oyster, c('Site.ID','Date.Collected','Quadrat.Number'))
fall2016.dead.oyster <- count(fall2016.dead.oyster, c('Site.ID','Date.Collected','Quadrat.Number'))

#Provide table with all the live and dead oysters counted per quadrat
fall2016.count.data <- oyster.data[,c(1,2,3,13,14,15)]
fall2016.count.data <- unique(count.data)

fall2016.live.count.data <- fall2016.count.data[,-6]
fall2016.dead.count.data <- fall2016.count.data[,-5]

#Merge measured counts and subsampled counts
fall2016.live.oyster.count <- merge(fall2016.live.oyster,fall2016.live.count.data, 
                           by = c('Site.ID','Date.Collected','Quadrat.Number'))
fall2016.dead.oyster.count <- merge(fall2016.dead.oyster,fall2016.dead.count.data, 
                           by = c('Site.ID','Date.Collected','Quadrat.Number'))

#add 2 counts together
fall2016.live.oyster.count$Total <- fall2016.live.oyster.count$freq+
  fall2016.live.oyster.count$Live.Oyster.Count
fall2016.dead.oyster.count$Total <- fall2016.dead.oyster.count$freq+
  fall2016.dead.oyster.count$Dead.Oyster.Count

#Rename and merge live/dead counts
fall2016.live.oyster.count <- fall2016.live.oyster.count[,-c(4,5,6)]
fall2016.dead.oyster.count <- fall2016.dead.oyster.count[,-c(4,5,6)]

colnames(fall2016.live.oyster.count) <- c('Site.ID','Date.Collected','Quadrat.Number','Total.Live')
colnames(fall2016.dead.oyster.count) <- c('Site.ID','Date.Collected','Quadrat.Number','Total.Dead')

fall2016.final.oyster.count <- merge(fall2016.live.oyster.count, fall2016.dead.oyster.count, 
                            by = c('Site.ID','Date.Collected','Quadrat.Number'))
fall2016.final.oyster.count$Total.All <- fall2016.final.oyster.count$Total.Live+
  fall2016.final.oyster.count$Total.Dead
fall2016.final.oyster.count$Pct.Alive <- fall2016.final.oyster.count$Total.Live/
  fall2016.final.oyster.count$Total.All*100

#Mean and std. error survival
fall2016.mean.oyster.survival <- aggregate(fall2016.final.oyster.count$Pct.Alive ~ 
                                             fall2016.final.oyster.count$Site.ID+
                                    fall2016.final.oyster.count$Date.Collected, FUN = mean)
fall2016.std.error.oyster.survival <- aggregate(fall2016.final.oyster.count$Pct.Alive ~ 
                                                  fall2016.final.oyster.count$Site.ID+
                                         fall2016.final.oyster.count$Date.Collected, FUN = se)
##rename columns
colnames(fall2016.mean.oyster.survival) <- c('Site.ID','Date.Collected','Mean.Pct.Alive')
colnames(fall2016.std.error.oyster.survival) <- c('Site.ID','Date.Collected','Standard.Error')

#merge
fall2016.oyster.survival <- merge(fall2016.mean.oyster.survival, 
                                  fall2016.std.error.oyster.survival, 
                                  by = c('Site.ID','Date.Collected'))

####Plot Fall 2016 Mean & SE Oyster Survival####
plot.fall2016.oyster.survival <- ggplot(fall2016.oyster.survival, aes(x=Site.ID, y=Mean.Pct.Alive)) + 
  geom_bar(position=position_dodge(), stat="identity", colour = "black") +
  geom_errorbar(aes(ymin=Mean.Pct.Alive-Standard.Error, ymax=Mean.Pct.Alive+Standard.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("% Survival of Oysters") + 
  xlab("Reef ID") + 
  ggtitle("Mean Survival of Remote-set Oysters on Reef w/ Std. Error")+
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 24),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 24),
        plot.title = element_text(size = 22))
plot.fall2016.oyster.survival
plot.fall2016.oyster.survival +
  coord_cartesian(ylim=c(80,100))