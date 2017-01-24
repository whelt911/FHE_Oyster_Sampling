####Oyster Size Distribution####

#Load packages
library("plyr")
library("sciplot")
library("ggplot2")
library("stats")

#Set working directory

#Load data
load("oyster.data")
load("quadrat.data")

# subset oysters for relevant sampling period
oyster.data <- subset(oyster.data, oyster.data$Date.Collected > '2016-8-1')
oyster.data <- subset(oyster.data, oyster.data$Date.Collected < '2016-12-31')

#Subset Live Only and Create list of sites with live oysters
live.oyster.data <- subset(oyster.data, oyster.data$Alive.Dead == "8")
live.oyster.sites <- unique(live.oyster.data$Site.ID)

####Histograms####

#vector for size bins
vec.breaks <- seq(from = 0, to = 105, by = 5)

#Histogram for all live oysters combined
hist(live.oyster.data$Size.mm, xlab = "Size (mm)",
     main = "Histogram of Live Oyster Size Distribution",
     col = "grey",
     breaks = vec.breaks)


#Histogram for live oysters by site
##Create lattice framework for hist plots
###Currently set up for 4 hist plots
ex.par <- par(mfrow = c(3,2))

#Loop that creates a histogram for each site with live oysters
for (i in 1:length(live.oyster.sites)){
  temp <- subset(live.oyster.data, live.oyster.data$Site.ID == live.oyster.sites[i])
  hist(temp$Size.mm, xlab = "Size (mm)", 
       main = paste("Histogram of Live Oyster Size Distribution", live.oyster.sites[i], sep = " "),
       col = "grey",
       breaks = vec.breaks)
}

####Mean + SE####
live.oyster.mean <- aggregate(live.oyster.data$Size.mm ~ live.oyster.data$Site.ID + 
                                live.oyster.data$Date.Collected, FUN = mean)
colnames(live.oyster.mean) <- c('Site.ID','Date.Collected','Mean.Length')

live.oyster.se <- aggregate(live.oyster.data$Size.mm ~ live.oyster.data$Site.ID + 
                              live.oyster.data$Date.Collected, FUN = se)
colnames(live.oyster.se) <- c('Site.ID','Date.Collected','Standard.Error')

live.oyster.total <- merge(live.oyster.mean, live.oyster.se, by = c('Site.ID','Date.Collected'))

####Live Oyster Size Mean + SE Plot####
plot.live.oyster.size.mean <- 
  ggplot(live.oyster.total, aes(x=Site.ID, y=Mean.Length)) + 
  geom_bar(position=position_dodge(), stat="identity", colour = "grey") +
  geom_errorbar(aes(ymin=Mean.Length-Standard.Error, ymax=Mean.Length+Standard.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("Mean Oyster Length (mm)") + 
  xlab("Reef ID") + 
  ggtitle("Mean Live Oyster Length on Reefs")+
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 24),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 24),
        plot.title = element_text(size = 32))
plot.live.oyster.size.mean

####Dead Oyster Size Mean + SE####

#Subset dead only and create list of sites with dead oysters
dead.oyster.data <- subset(oyster.data, oyster.data$Alive.Dead == "9")
dead.oyster.sites <- unique(dead.oyster.data$Site.ID)

#Histogram for all dead oysters combined
par1 <- par(mfrow = c(1,1))

hist(dead.oyster.data$Size.mm, xlab = "Size (mm)",
     main = "Histogram of Dead Oyster Size Distribution",
     col = "grey",
     breaks = vec.breaks)

#Histogram of dead oyster size distribution by site
##Create lattice framework for hist plots
###Currently set up for 4 hist plots
ex.par <- par(mfrow = c(2,2))

#Loop that creates a histogram for each site with dead oysters
for (i in 1:length(dead.oyster.sites)){
  temp <- subset(dead.oyster.data, dead.oyster.data$Site.ID == dead.oyster.sites[i])
  hist(temp$Size.mm, xlab = "Size (mm)", 
       main = paste("Histogram of Dead Oyster Size Distribution", dead.oyster.sites[i], sep = " "),
       col = "grey",
       breaks = vec.breaks)
}

####Mean + SE####
dead.oyster.mean <- aggregate(dead.oyster.data$Size.mm ~ dead.oyster.data$Site.ID + 
                                dead.oyster.data$Date.Collected, FUN = mean)
colnames(dead.oyster.mean) <- c('Site.ID','Date.Collected','Mean.Length')

dead.oyster.se <- aggregate(dead.oyster.data$Size.mm ~ dead.oyster.data$Site.ID + 
                              dead.oyster.data$Date.Collected, FUN = se)
colnames(dead.oyster.se) <- c('Site.ID','Date.Collected','Standard.Error')

dead.oyster.total <- merge(dead.oyster.mean, dead.oyster.se, by = c('Site.ID','Date.Collected'))

####dead Oyster Size Mean + SE Plot####
plot.dead.oyster.size.mean <- 
  ggplot(dead.oyster.total, aes(x=Site.ID, y=Mean.Length)) + 
  geom_bar(position=position_dodge(), stat="identity", colour = "grey") +
  geom_errorbar(aes(ymin=Mean.Length-Standard.Error, ymax=Mean.Length+Standard.Error),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("Mean Oyster Length (mm)") + 
  xlab("Reef ID") + 
  ggtitle("Mean Recently Dead Oyster Length on Reefs")+
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 24),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 24),
        plot.title = element_text(size = 32))
plot.dead.oyster.size.mean

####Shapiro-Wilk test for normality on size distribution####
shapiro.test(live.oyster.data$Size.mm)
hist(live.oyster.data$Size.mm)

####Anova comparing size distribution across sites
anova1 <-aov(live.oyster.data$Size.mm ~ live.oyster.data$Site.ID)
summary(anova1)
TukeyHSD(anova1)
