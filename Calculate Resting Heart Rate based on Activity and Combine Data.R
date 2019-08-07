#### ALL DC SA2 and GL TOgether
#This piece of code combines the demographic data with Hr data and put them all in a data frame


ls()
rm(list=ls())
library(ggplot2)
#read Data
#demo is demographic data
demo<-read.csv ("C:/Users/m7979/Downloads/PTSD Data/San Antonio 2/DemoCleanSA.csv", header=TRUE)
attach(demo)
#Extract device numbers and take the three last chars
demonames <- as.character(Device.Number)
demonames1 <- substr(demonames, nchar(demonames)-2, nchar(demonames))


#m <- matrix(NA, nrow = 23, byrow = TRUE)
#Storing all the files' names in the folder 

fileNames <- Sys.glob("C:/Users/m7979/Downloads/PTSD Data/San Antonio 2/SA2/*.csv")
i<- 1

p<- list.files("C:/Users/m7979/Downloads/PTSD Data/San Antonio 2/SA2", pattern="*.csv", full.names=TRUE)

total <- data.frame()

for (i in 1:36) {
  
  #data is heart rate data
  data <- read.csv (filename <- p[[i]])
  
  #Extract file name but it has .CSV
  name <- basename(p[[i]])
  
  
  #m[i,1] <- name
  #i<= i+1
  #Take out the part with CSV name
  name <- gsub (".CSV", "", name, ignore.case=TRUE)
  
  #Take the 3 last characters
  name<-substr(name, nchar(name)-2, nchar(name))
  
  #Device Number that matches up with the file name
  n <-grep(name, demonames1)
  #if its not in the demo then move to the next one
  if (length(n)==0) {next}
  #put the n row (the one that matches up) in a new dataframe
  x <- demo[n,]
  
  
  
  #omit missing values of HR
  ndata <- data [!is.na(data$hr),]
  ndata <- ndata[!ndata$hr<50,]
  #Defining a new data frame

  ndata$totallinearacc <- sqrt ((ndata$linear_accel_x^2)+(ndata$linear_accel_y^2)+(ndata$linear_accel_z^2))
  
  rdata <- ndata[!ndata$totallinearacc>1, ]
  Rheartrate <- mean(rdata$hr, na.rm=TRUE) 
  ndata <- ndata[!is.na(ndata$ptsd_moment),]
  stress <- nrow(ndata)
  if (stress==0) {next}
  #mean(ndata$hr)
  
  
  #start date is the date they had the watch
  startdate <- head(ndata,1)
  #enddate is the last record
  enddate <- tail (ndata,1)
  
  #average heart rate in stress moments
  hravg <- mean (ndata$hr)
  
  #avg acc in each measure
  accxavg <- mean (ndata$acc_x)
  accyavg <- mean (ndata$acc_y)
  acczavg <- mean (ndata$acc_z)
  accxavgLin <- mean (ndata$linear_accel_x)
  accyavgLin <- mean (ndata$linear_accel_y)
  acczavgLin <- mean (ndata$linear_accel_z)
  Accvector <- sqrt ((accxavg^2)+(accyavg^2)+(acczavg^2)  )
  Accvector <- sqrt ((accxavgLin^2)+(accyavgLin^2)+(acczavgLin^2)  )
  
  #put all the data in the datframe
  
  T  <- data.frame (i,x,startdate$date, startdate$time, enddate$date, enddate$time,accxavg, accyavg, acczavg,accxavgLin, accyavgLin, acczavgLin,Accvector,Accvector, Rheartrate,hravg,stress)
  
  total <- rbind (total,T)
}


#Save the file in the directory
write.csv(total, file="MydataSA.csv")









ls()
rm(list=ls())
#read Data
#demo is demographic data
demo<-read.csv ("C:/Users/m7979/Downloads/PTSD Data/DC Data/DC-20181010T215944Z-001/DC.csv", header=TRUE)
attach(demo)
#Extract device numbers and take the three last chars
demonames <- as.character(Device.Number)
demonames1 <- substr(demonames, nchar(demonames)-2, nchar(demonames))


#m <- matrix(NA, nrow = 23, byrow = TRUE)
#Storing all the files' names in the folder 

fileNames <- Sys.glob("C:/Users/m7979/Downloads/PTSD Data/DC Data/DC-20181010T215944Z-001/DC/*.csv")
i<- 1

p<- list.files("C:/Users/m7979/Downloads/PTSD Data/DC Data/DC-20181010T215944Z-001/DC", pattern="*.csv", full.names=TRUE)

total <- data.frame()

for (i in 1:21) {
  
  #data is heart rate data
  data <- read.csv (filename <- p[[i]])
  
  #Extract file name but it has .CSV
  name <- basename(p[[i]])
  
  
  #m[i,1] <- name
  #i<= i+1
  #Take out the part with CSV name
  name <- gsub (".CSV", "", name, ignore.case=TRUE)
  
  #Take the 3 last characters
  name<-substr(name, nchar(name)-2, nchar(name))
  
  #Device Number that matches up with the file name
  n <-grep(name, demonames1)
  #if its not in the demo then move to the next one
  if (length(n)==0) {next}
  #put the n row (the one that matches up) in a new dataframe
  x <- demo[n,]
  
  
  
  #omit missing values of HR
  ndata <- data [!is.na(data$hr),]
  ndata <- ndata[!ndata$hr<50,]
  #Defining a new data frame
  ndata$totallinearacc <- sqrt ((ndata$linear_accel_x^2)+(ndata$linear_accel_y^2)+(ndata$linear_accel_z^2))
  
  rdata <- ndata[!ndata$totallinearacc>1, ]
  
  Rheartrate <- mean(rdata$hr, na.rm=TRUE) 
  
  
  
  ndata <- ndata[!is.na(ndata$ptsd_moment),]
  stress <- nrow(ndata)
  if (stress==0) {next}
  #mean(ndata$hr)
  
  
  #start date is the date they had the watch
  startdate <- head(ndata,1)
  #enddate is the last record
  enddate <- tail (ndata,1)
  
  #average heart rate in stress moments
  hravg <- mean (ndata$hr)
  
  #avg acc in each measure
  accxavg <- mean (ndata$acc_x)
  accyavg <- mean (ndata$acc_y)
  acczavg <- mean (ndata$acc_z)
  accxavgLin <- mean (ndata$linear_accel_x)
  accyavgLin <- mean (ndata$linear_accel_y)
  acczavgLin <- mean (ndata$linear_accel_z)
  Accvector <- sqrt ((accxavg^2)+(accyavg^2)+(acczavg^2)  )
  Accvector <- sqrt ((accxavgLin^2)+(accyavgLin^2)+(acczavgLin^2)  )
  
  #put all the data in the datframe
  
  T  <- data.frame (i,x,startdate$date, startdate$time, enddate$date, enddate$time,accxavg, accyavg, acczavg,accxavgLin, accyavgLin, acczavgLin,Accvector,Accvector, Rheartrate,hravg,stress)
  
  total <- rbind (total,T)
}


write.csv(total, file="MydataDC.csv")
#Note: when there is no stress moment it returns NAN for hr mean







ls()
rm(list=ls())
#read Data
#demo is demographic data
demo<-read.csv ("C:/Users/m7979/Downloads/PTSD Data/GL Data/GLDemo.CSV", header=TRUE)
attach(demo)
#Extract device numbers and take the three last chars
demonames <- as.character(Device.Number)
demonames1 <- substr(demonames, nchar(demonames)-2, nchar(demonames))


fileNames <- Sys.glob("C:/Users/m7979/Downloads/PTSD Data/GL Data/GL/*.csv")
i<- 1
p<- list.files("C:/Users/m7979/Downloads/PTSD Data/GL Data/GL", pattern="*.csv", full.names=TRUE)

total <- data.frame()
for (i in 1:12) {
  
  
  #data is heart rate data
  data <- read.csv (filename <- p[[i]])
  
  #Extract file name but it has .CSV
  name <- basename(p[[i]])
  
  
  #m[i,1] <- name
  #i<= i+1
  #Take out the part with CSV name
  name <- gsub (".CSV", "", name, ignore.case=TRUE)
  
  #Take the 3 last characters
  name<-substr(name, nchar(name)-2, nchar(name))
  
  #Device Number that matches up with the file name
  n <-grep(name, demonames1)
  #if its not in the demo then move to the next one
  if (length(n)==0) {next}
  #put the n row (the one that matches up) in a new dataframe
  x <- demo[n,]
  
  
  
  #omit missing values of HR
  ndata <- data [!is.na(data$hr),]
  ndata <- ndata[!ndata$hr<50,]
  #Defining a new data frame
  hrdata <- ndata$hr
  #Defining a new data frame
  ndata$totallinearacc <- sqrt ((ndata$linear_accel_x^2)+(ndata$linear_accel_y^2)+(ndata$linear_accel_z^2))
  
  rdata <- ndata[!ndata$totallinearacc>1, ]
  
  Rheartrate <- mean(rdata$hr, na.rm=TRUE) 
  
  
  
  ndata <- ndata[!is.na(ndata$ptsd_moment),]
  stress <- nrow(ndata)
  if (stress==0) {next}
  #mean(ndata$hr)
  
  
  #start date is the date they had the watch
  startdate <- head(ndata,1)
  #enddate is the last record
  enddate <- tail (ndata,1)
  
  #average heart rate in stress moments
  hravg <- mean (ndata$hr)
  
  #avg acc in each measure
  accxavg <- mean (ndata$acc_x)
  accyavg <- mean (ndata$acc_y)
  acczavg <- mean (ndata$acc_z)
  accxavgLin <- mean (ndata$linear_accel_x)
  accyavgLin <- mean (ndata$linear_accel_y)
  acczavgLin <- mean (ndata$linear_accel_z)
  Accvector <- sqrt ((accxavg^2)+(accyavg^2)+(acczavg^2)  )
  Accvector <- sqrt ((accxavgLin^2)+(accyavgLin^2)+(acczavgLin^2)  )
  
  #put all the data in the datframe
  
  T  <- data.frame (i,x,startdate$date, startdate$time, enddate$date, enddate$time,accxavg, accyavg, acczavg,accxavgLin, accyavgLin, acczavgLin,Accvector,Accvector, Rheartrate,hravg,stress)
  
  total <- rbind (total,T)
}


write.csv(total, file="MydataGreatLakes.csv")
