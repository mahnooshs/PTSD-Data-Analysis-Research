#### In this code I am making a big data frame that has all stress moments for everyone and 
#and heart rate at the moment
#and activity and time of the day
#I wanna see the distribution of heart rate in these moments and affect of day/night in it



ls()
rm(list=ls())
#read Data

###########################################DC################################DC################


fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/DC Data/DC-20181010T215944Z-001/DC/*.csv")
i<- 1

p<- list.files("Desktop/PTSD/PTSD Data/DC Data/DC-20181010T215944Z-001/DC", pattern="*.csv", full.names=TRUE)
A <- "DC"
total <- data.frame()
for (i in 1:21) {
  #reading in data
  data <- read.csv (filename <- p[[i]])
  ndata <- data [!is.na(data$ptsd_moment),]
  #ndata <- ndata[!ndata$hr==0,]
  if (nrow(ndata)==0) {next}
  #put all the data in the datframe
  ndata$i = i
  ndata$zone=A
  total<- rbind (total,ndata)
}



#############GL############
fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/GL Data/GL/*.csv")
i<- 1

p<- list.files("Desktop/PTSD/PTSD Data/GL Data/GL", pattern="*.csv", full.names=TRUE)
A <- "GL"
#totalDC <- data.frame()
for (i in 1:12) {
  #reading in data
  data <- read.csv (filename <- p[[i]])
  ndata <- data [!is.na(data$ptsd_moment),]
  #ndata <- ndata[!ndata$hr==0,]
  if (nrow(ndata)==0) {next}
  #put all the data in the datframe
  ndata$i = i
  ndata$zone=A
  total<- rbind (total,ndata)
}


##############################CALIFORNIA######################################################################

fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/California/drive-download-20181204T172324Z-001/*.csv")
i<- 1

p<- list.files("Desktop/PTSD/PTSD Data/California/drive-download-20181204T172324Z-001", pattern="*.csv", full.names=TRUE)
A <- "CALI"
#totalDC <- data.frame()
for (i in 1:21) {
  #reading in data
  data <- read.csv (filename <- p[[i]])
  ndata <- data [!is.na(data$ptsd_moment),]
  #ndata <- ndata[!ndata$hr==0,]
  if (nrow(ndata)==0) {next}
  #put all the data in the datframe
  ndata$i = i
  ndata$zone=A
  total<- rbind (total,ndata)
}


##############################LASVEGAS#############################################################

fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/LasVegas/drive-download-20181204T171643Z-001/*.csv")
i<- 1

p<- list.files("Desktop/PTSD/PTSD Data/LasVegas/drive-download-20181204T171643Z-001", pattern="*.csv", full.names=TRUE)
A <- "LASVEGAS"
for (i in 1:8) {
  #reading in data
  data <- read.csv (filename <- p[[i]])
  ndata <- data [!is.na(data$ptsd_moment),]
  #ndata <- ndata[!ndata$hr==0,]
  if (nrow(ndata)==0) {next}
  #put all the data in the datframe
  ndata$i = i
  ndata$zone=A
  total <- rbind (total,ndata)
}


##############################SANANTONIO2############################################################

fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/San Antonio 2/SA2/*.csv")
i<- 1

p<- list.files("Desktop/PTSD/PTSD Data/San Antonio 2/SA2", pattern="*.csv", full.names=TRUE)
A <- "SANANTONIO"
for (i in 1:36) {
  #reading in data
  data <- read.csv (filename <- p[[i]])
  ndata <- data [!is.na(data$ptsd_moment),]
  #ndata <- ndata[!ndata$hr==0,]
  if (nrow(ndata)==0) {next}
  #put all the data in the datframe
  ndata$i = i
  ndata$zone=A
  total <- rbind (total,ndata)
}

#Saving the aggregated file --> moved it later to PTSD Descriptive (for descriptive paper ) Folder

write.csv(total,"Desktop/PTSD/PTSD Data/stressmoments.csv", row.names = FALSE)


#Read in the data so that you do not have to redo it
total = read.csv("Desktop/PTSD/PTSD Data/PTSD Descriptive (for Descriptive paper)/stressmoments.csv")

#count number of people reported stress moments in each zone, for instance in DC 20 people reported stress.
library(data.table)
setDT(total)[, .(count = uniqueN(i)), by = zone]

#heart rate mean without zeros and NA

hrlist <- total$hr [!total$hr==0]
sort (hrlist)
mean (hrlist, na.rm =  TRUE)
sd(hrlist,na.rm = TRUE)

# Kernel Density Plot
d <- density(hrlist, na.rm = TRUE) # returns the density data 
plot(d, main="",
     xlab="Heart Rate",
     ylab="Density",) # plots the results

hist(hrlist, main="",
     xlab="Heart Rate",
     ylab="Density",labels=TRUE) 

##Ploting with ggplot
library(ggplot2)
library(plyr)
heartrate <- ldply (hrlist, data.frame)
names(heartrate)[names(heartrate) == "X..i.."] <- "hr"
theme_set(theme_bw((base_size=24)))
ggplot(heartrate, aes(x= hr)) + 
  geom_density(size=1.5, fill='grey') + xlab('Heart rate during reported stress moments')

ggsave('Heart Rate Density grey.pdf', dpi=300)
ggsave('Heart Rate Density grey.png', dpi=300)

#### Plotting time vs stress moments

minute <- as.POSIXlt(as.character(total$time), format="%H:%M:%S")
plot( minute, total$ptsd_moment, xlab="Time", ylab="Stress")
options(scipen=999)
hist(minute , breaks = "hours",labels=TRUE, freq = TRUE,
     xlab="Hour of Day",ylab="Frequency of Stress Moments", main="")


#minute1 <- as.POSIXct(as.character(total$time), format="%H:%M:%S")


##Plot of heart rate measures in stress moments
plot( minute[!total$hr==0], total$hr[!total$hr==0], xlab="Time", ylab="Heart Rate in Stress Moments")

#ggplot(total, 
      # aes(x=minute1, y=total$hr)) + geom_point()

####Activity
total$linearacc = sqrt((total$linear_accel_x^2)+(total$linear_accel_y^2)+(total$linear_accel_z^2))

total$linearacc[is.na(total$linearacc)] <- 0
total$linear_accel_x[is.na(total$linear_accel_x)] <- 0

total$activity=NA

for (i in 1:1023){
  if (total$linear_accel_x[i] > 1.3) 
    {total$activity[i] <- "Active"}
  else (total$activity[i] <- "Resting")
} 

library(ggplot2)

#Activity plot vs stress count

dodge <- position_dodge(width = 0.5)
theme_set(theme_bw((base_size=24)))
ggplot(total,
       aes(factor(activity))) +
  geom_bar(fill = "black",
           alpha = 1, width=0.2) + ggtitle("") +
  xlab("Activity") + ylab("Number of stress moments reported")

ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(colour="black", stat="identity")

table(total$activity)

m = matrix(c('Active','Resting', 133,890), nrow = 2, ncol=2)
m = data.frame(m)
names(m)[names(m) == "X1"] <- "Activity"
ggplot(data=m, aes(x=Activity, y=X2, fill=Activity)) +
  geom_bar(colour="black", stat="identity")+ ggtitle("") +
  xlab("Activity") + ylab("Number of stress moments reported")+
  #scale_fill_brewer(palette="Greys")
  scale_fill_manual(values=c('black',"light grey" ))

ggsave('Activity.pdf', dpi=300)
ggsave('Activity.png', dpi=300)
#ggsave('Activity.pdf')



##






