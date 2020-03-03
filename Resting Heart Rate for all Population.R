###This piece of code calculates resting heart rate for all participants that we have data from based on their activity level
## The output is saved in PTSD data in resting heart rate for all folder
ls()
rm(list=ls())
library(ggplot2)
#read Data

#Storing all the files' names in the folder 

fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/San Antonio 2/SA2/*.csv")
i<- 1

p<- list.files("Desktop/PTSD/PTSD Data/San Antonio 2/SA2", pattern="*.csv", full.names=TRUE)

total <- data.frame()

x="SA2"
for (i in 1:36) {
  
  #data is heart rate data
  data <- read.csv (filename <- p[[i]])
  
  #omit missing values of HR
  ndata <- data [!is.na(data$hr),]
  ndata <- ndata[!ndata$hr<50,]
  #Defining a new data frame
  
  ndata$totallinearacc <- sqrt ((ndata$linear_accel_x^2)+(ndata$linear_accel_y^2)+(ndata$linear_accel_z^2))
  
  rdata <- ndata[!ndata$totallinearacc>1, ]
  Rheartrate <- mean(rdata$hr, na.rm=TRUE) 
  
  #start date is the date they had the watch
  startdate <- head(ndata,1)
  #enddate is the last record
  enddate <- tail (ndata,1)
  
  #average heart rate in stress moments
  hravg <- mean (ndata$hr)
  
  #put all the data in the datframe
  
  T  <- data.frame (i,x,startdate$date, startdate$time, enddate$date, enddate$time, Rheartrate,hravg)
  
  total <- rbind (total,T)
}








###DC

fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/DC Data/DC-20181010T215944Z-001/DC/*.csv")
i<- 1

p<- list.files("Desktop/PTSD/PTSD Data/DC Data/DC-20181010T215944Z-001/DC", pattern="*.csv", full.names=TRUE)
x="DC"
for (i in 1:21) {
  
  #data is heart rate data
  data <- read.csv (filename <- p[[i]])
  
  #omit missing values of HR
  ndata <- data [!is.na(data$hr),]
  ndata <- ndata[!ndata$hr<50,]
  #Defining a new data frame
  
  ndata$totallinearacc <- sqrt ((ndata$linear_accel_x^2)+(ndata$linear_accel_y^2)+(ndata$linear_accel_z^2))
  
  rdata <- ndata[!ndata$totallinearacc>1, ]
  Rheartrate <- mean(rdata$hr, na.rm=TRUE) 
  
  #start date is the date they had the watch
  startdate <- head(ndata,1)
  #enddate is the last record
  enddate <- tail (ndata,1)
  
  #average heart rate in stress moments
  hravg <- mean (ndata$hr)
  
  #put all the data in the datframe
  
  T  <- data.frame (i,x,startdate$date, startdate$time, enddate$date, enddate$time, Rheartrate,hravg)
  
  total <- rbind (total,T)
}






##GL

fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/GL Data/GL/*.csv")
i<- 1
p<- list.files("Desktop/PTSD/PTSD Data/GL Data/GL", pattern="*.csv", full.names=TRUE)
x="Great Lakes"
for (i in 1:12) {
  #data is heart rate data
  data <- read.csv (filename <- p[[i]])
  
  #omit missing values of HR
  ndata <- data [!is.na(data$hr),]
  ndata <- ndata[!ndata$hr<50,]
  #Defining a new data frame
  
  ndata$totallinearacc <- sqrt ((ndata$linear_accel_x^2)+(ndata$linear_accel_y^2)+(ndata$linear_accel_z^2))
  
  rdata <- ndata[!ndata$totallinearacc>1, ]
  Rheartrate <- mean(rdata$hr, na.rm=TRUE) 
  
  #start date is the date they had the watch
  startdate <- head(ndata,1)
  #enddate is the last record
  enddate <- tail (ndata,1)
  
  #average heart rate in stress moments
  hravg <- mean (ndata$hr)
  
  #put all the data in the datframe
  
  T  <- data.frame (i,x,startdate$date, startdate$time, enddate$date, enddate$time, Rheartrate,hravg)
  
  total <- rbind (total,T)
}







##############################CALIFORNIA######################################################################

fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/California/drive-download-20181204T172324Z-001/*.csv")
i<- 1

p<- list.files("Desktop/PTSD/PTSD Data/California/drive-download-20181204T172324Z-001", pattern="*.csv", full.names=TRUE)
x <- "California"

for (i in 1:21) {
  data <- read.csv (filename <- p[[i]])
  
  #omit missing values of HR
  ndata <- data [!is.na(data$hr),]
  ndata <- ndata[!ndata$hr<50,]
  #Defining a new data frame
  
  ndata$totallinearacc <- sqrt ((ndata$linear_accel_x^2)+(ndata$linear_accel_y^2)+(ndata$linear_accel_z^2))
  
  rdata <- ndata[!ndata$totallinearacc>1, ]
  Rheartrate <- mean(rdata$hr, na.rm=TRUE) 
  
  #start date is the date they had the watch
  startdate <- head(ndata,1)
  #enddate is the last record
  enddate <- tail (ndata,1)
  
  #average heart rate in stress moments
  hravg <- mean (ndata$hr)
  
  #put all the data in the datframe
  
  T  <- data.frame (i,x,startdate$date, startdate$time, enddate$date, enddate$time, Rheartrate,hravg)
  
  total <- rbind (total,T)
}



######Vegas#
fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/LasVegas/drive-download-20181204T171643Z-001/*.csv")
i<- 1

p<- list.files("Desktop/PTSD/PTSD Data/LasVegas/drive-download-20181204T171643Z-001", pattern="*.csv", full.names=TRUE)
x <- "LASVEGAS"
for (i in 1:8) {
  data <- read.csv (filename <- p[[i]])
  
  #omit missing values of HR
  ndata <- data [!is.na(data$hr),]
  ndata <- ndata[!ndata$hr<50,]
  #Defining a new data frame
  
  ndata$totallinearacc <- sqrt ((ndata$linear_accel_x^2)+(ndata$linear_accel_y^2)+(ndata$linear_accel_z^2))
  
  rdata <- ndata[!ndata$totallinearacc>1, ]
  Rheartrate <- mean(rdata$hr, na.rm=TRUE) 
  
  #start date is the date they had the watch
  startdate <- head(ndata,1)
  #enddate is the last record
  enddate <- tail (ndata,1)
  
  #average heart rate in stress moments
  hravg <- mean (ndata$hr)
  
  #put all the data in the datframe
  
  T  <- data.frame (i,x,startdate$date, startdate$time, enddate$date, enddate$time, Rheartrate,hravg)
  
  total <- rbind (total,T)
}

write.csv(total, file="RestingHeartRateAll.csv")

#Read in the data from the saved file
total = read.csv("Desktop/PTSD/PTSD Data/Resting heart rate for all/RestingHeartRateAll.csv")


sort (total$Rheartrate)
mean (total$Rheartrate, na.rm =  TRUE)
sd(total$Rheartrate,na.rm = TRUE)


# Kernel Density Plot
d <- density(total$Rheartrate, na.rm = TRUE) # returns the density data 
plot(d, main="",
     xlab="Resting Hear Rate",
     ylab="Density",) # plots the results

hist(total$Rheartrate, main="",
     xlab="Resting Hear Rate",
     ylab="Frequency",labels=TRUE) 

library(ggplot2)
theme_set(theme_bw((base_size=24)))
ggplot(total, aes(x= Rheartrate)) + 
  geom_density(size=1.5, fill='grey') + xlab('Resting heart rate')+
  ylab('Density')


ggsave('Resting Heart Rate Density.pdf', dpi=300)
ggsave('Resting Heart Rate Density.png', dpi=300)



