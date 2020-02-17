#Looking at 30 seconds windows, 60 seconds windows and 15 seconds windows for descriptive paper


ls()
rm(list=ls())


library(plot3D)
library(rgl)
library(car)

data<-read.csv ("Desktop/PTSD/PTSD Data/HR Features/HR Features_dataframe.csv", header=TRUE)
data<-data[data$ptsd_moment==1,]
attach (data)

############MEAN HEART RATE DENSITY FOR WINDOWS OF 60 SECONDS#####
d <- density(data$hrmean, na.rm = TRUE) # returns the density data 
plot(d, main="",
     xlab="Heart Rate Average",
     ylab="Density",) # plots the results

#Histogram with counts
#If you wanna show numbers you should say ,labels=TRUE
hist(data$hrmean, main="",
     xlab="Heart Rate Average",
     ylab="Density") 

#Histogram with density
h = hist(hrmean, plot = FALSE) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE,main="",
     xlab="Heart Rate Average",
     ylab="Density")



############MINIMUM HEART RATE DENSITY FOR WINDOWS OF 60 SECONDS#####
d <- density(data$hrmin, na.rm = TRUE) # returns the density data 
plot(d, main="",
     xlab="Minimum Heart Rate",
     ylab="Density",) # plots the results

#Histogram with counts
#If you wanna show numbers you should say ,labels=TRUE
hist(data$hrmin, main="",
     xlab="Minimum Heart Rate",
     ylab="Density") 

#Histogram with density
h = hist(hrmin, plot = FALSE) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE,main="",
     xlab="Minimum Heart Rate",
     ylab="Density")




############STD HEART RATE DENSITY FOR WINDOWS OF 60 SECONDS#####
d <- density(data$hrsd, na.rm = TRUE) # returns the density data 
plot(d, main="",
     xlab="Heart Rate Standard Deviation",
     ylab="Density",) # plots the results

#Histogram with counts
#If you wanna show numbers you should say ,labels=TRUE
hist(data$hrsd, main="",
     xlab="Heart Rate Standard Deviation",
     ylab="Density") 

#Histogram with density
h = hist(hrsd, plot = FALSE) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE,main="",
     xlab="Heart Rate Standard Deviation",
     ylab="Density")



############MAXIMUM HEART RATE DENSITY FOR WINDOWS OF 60 SECONDS#####
d <- density(data$hrmax, na.rm = TRUE) # returns the density data 
plot(d, main="",
     xlab="Maximum Heart Rate",
     ylab="Density",) # plots the results

#Histogram with counts
#If you wanna show numbers you should say ,labels=TRUE
hist(data$hrmax, main="",
     xlab="Maximum Heart Rate",
     ylab="Density") 

#Histogram with density
h = hist(hrmax, plot = FALSE) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE,main="",
     xlab="Maximum Heart Rate",
     ylab="Density")




############Range HEART RATE DENSITY FOR WINDOWS OF 60 SECONDS#####
d <- density(data$hrrange, na.rm = TRUE) # returns the density data 
plot(d, main="",
     xlab="Heart Rate Range",
     ylab="Density",) # plots the results

#Histogram with counts
#If you wanna show numbers you should say ,labels=TRUE
hist(data$hrrange, main="",
     xlab="Heart Rate Range",
     ylab="Density") 

#Histogram with density
h = hist(hrrange, plot = FALSE) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE,main="",
     xlab="Heart Rate Range",
     ylab="Density")



######3d Plots#


with(data, plot3d(hrmin,hrmax,hrmean,type = 's', col=as.integer(hrsd)+10))


# 3D plot with the regression plane
scatter3d(x = hrmin, y =hrmax, z = hrmean)
scatter3D(x = hrmin, y =hrmax, z = hrmean, clab = c("Sepal", "Width (cm)"))

