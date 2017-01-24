####Oyster Data Import & Cleanup####

#Load Packages


#Set working dir
setwd("H:/Will's Drive/R_wd/fhe/FHE_Oyster_Sampling")

#Import data
oyster.data <- read.csv("Oyster_Data.txt")
quadrat.data <- read.csv("Quadrat_Data.txt")

#Clean up and merge datasets

##merge quadrat & oyster data into oyster data
quadrat.data <- quadrat.data[,-7]
oyster.data <- merge(oyster.data,quadrat.data, by = c('Site.ID','Date.Collected','Quadrat.Number'))

##Clean up Date Collected column 
oyster.data$Date.Collected <- as.Date(oyster.data$Date.Collected, format='%m/%d/%Y')
quadrat.data$Date.Collected <- as.Date(quadrat.data$Date.Collected, format='%m/%d/%Y')





#Save datasets
save(oyster.data, file = 'oyster.data')
save(quadrat.data, file = 'quadrat.data')
