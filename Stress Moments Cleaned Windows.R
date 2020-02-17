##############Don't use this code, there is a mistake in one of the files####


ls()
rm(list=ls())
#read Data



fileNames <- Sys.glob("Desktop/PTSD/PTSD Data/Cleaned for stress moments/All/*.csv")

p<- list.files("Desktop/PTSD/PTSD Data/Cleaned for stress moments/All", pattern="*.csv", full.names=TRUE)

total <- data.frame()
for (i in 1:88) {
  #reading in data
  data <- read.csv (filename <- p[[i]])
  total<- rbind (total,data)
}

###KAlman Imputation#
library(data.table)
library(imputeTS)
library(varhandle)
total$hr[total$hr==0]= NA
total$hr[total$hr=='*']= NA
total$hr1 = unfactor(total$hr)
total$hr1=na_kalman(total$hr1, model = "StructTS", smooth = TRUE, nit = -1, maxgap = 5)


total1 = total[!(total$date == '*'),]
total1$ptsd_moment = unfactor(total1$ptsd_moment)
total1$ptsd_moment[!is.na(total1$ptsd_moment)]= 1
total1$ptsd_moment[is.na(total1$ptsd_moment)]= 0

trial= total1[40201:50250,]

j=1
k=1
trial$windowno=0
while (j< nrow(trial)) {
  l=j+201
  trial[j:l, 'windowno']=k
  j=l
  k=k+1
}

