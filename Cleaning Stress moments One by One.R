

ls()
rm(list=ls())
#read Data


p<- list.files("Desktop/PTSD/PTSD Data/Five mins windows of stress moments after stress", 
               pattern="*.csv", full.names=TRUE)

#####i17 hanuz anjam nadadam
total1 <- data.frame()

i=1

i=i+1
data = read.csv (filename <- p[[i]])

total <- data.frame()

data1 = data [data$windowno ==4,]

#total1<-read.csv ("Desktop/PTSD/PTSD Data/CLEANEDFive mins windows of stress moments after stress/CleanedStressnotall.csv", header=TRUE)

total<- rbind (total,data1)

#setwd("/Users/mahnooshsadeghi")

write.csv(total, file="CleanedStressp20.csv")

#total<- rbind (total,data1)

#total1 = rbind(total, total1)
#write.csv(total1, file="CleanedStressnotall.csv")
