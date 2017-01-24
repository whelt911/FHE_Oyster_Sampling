####Test for % algal cover on reefs vs. control####

#working directory
setwd("Z:/Will's Drive/R_wd/fhe/FHE_Oyster_Sampling")

#packages
library('sciplot')
library('ggplot2')
library('stats')

#read in dataset
dat <- read.csv("Quadrat_Data.txt")
dat <- dat[,-c(5,7,8,9,10)]

####Summarize data####
a <- aggregate(dat$X..Algal.Cover ~ dat$Site.ID, FUN = mean)
b <- aggregate(dat$X..Algal.Cover ~ dat$Site.ID, FUN = se)

avg <- merge(a,b, by = 'dat$Site.ID')
colnames(avg) <- c('Site.ID','Mean','SE')

####plot summarized data####
summ.barplot <- ggplot(c, aes(x=Site.ID, y=Mean)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("Avg.% Coverage") + 
  xlab("Site ID") + 
  ggtitle("Avg. Percent Cover of Macroalgae at FHE Sites")
summ.barplot

####Shapiro-Wilk -- check for normal distribution####
shapiro.test(dat$X..Algal.Cover)
