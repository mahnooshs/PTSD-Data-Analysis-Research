#This excel file does not have 0 for count of stress

ls()
rm(list=ls())
library(ggplot2)
library(MASS)
library(CARS)
library(tidyverse)
library(caret)
library(leaps)
library(plotly)
library(glmnet)
library(MXM)
library(olsrr)
library(ggcorrplot)
library(plot3D)
library(rgl)
library(car)
#read Data
#demo is demographic data
data<-read.csv ("Desktop/PTSD/PTSD Data/New/Data demographics for SA DC GL/CombinedData.csv", header=TRUE)


subdata <-data[,c("Age","Gender", "Anti.depressants","Anxiolytics", "Glucocorticoids", "Smoke", "Alcohol", "Exercise", "Sleep", "accxavg","accyavg","acczavg","Accvector","Rheartrate","hravg","stress")]


#trigger is log stress for poisson distribution
subdata$trigger = log(subdata$stress+1)
corr = cor(subdata)

fit <- lm(data$stress~data$Rheartrate)
summary(fit)
ggcorrplot(cor(subdata), type = "lower",
           outline.col = "white")

ggcorrplot(corr, type = "lower",
           lab = TRUE)


#Compute missing value
subdata = transform(subdata,  accxavg= ifelse(is.na(accxavg), mean(accxavg, na.rm=TRUE), accxavg))
subdata = transform(subdata,  accyavg= ifelse(is.na(accyavg), mean(accyavg, na.rm=TRUE), accyavg))
subdata = transform(subdata,  acczavg= ifelse(is.na(acczavg), mean(acczavg, na.rm=TRUE), acczavg))
#subdata = transform(subdata,  accxavgLin= ifelse(is.na(accxavgLin), mean(accxavgLin, na.rm=TRUE), accxavgLin))
#subdata = transform(subdata,  accyavgLin= ifelse(is.na(accyavgLin), mean(accyavgLin, na.rm=TRUE), accyavgLin))
#subdata = transform(subdata,  acczavgLin= ifelse(is.na(acczavgLin), mean(acczavgLin, na.rm=TRUE), acczavgLin))
subdata = transform(subdata,  Accvector= ifelse(is.na(Accvector), mean(Accvector, na.rm=TRUE), Accvector))

# Fit the full model 
full.model <- lm(hravg ~., data = subdata)

step1 <- stepAIC(full.model, direction="both")

attach(subdata)
model = lm(hravg ~ Anti.depressants +  Smoke+ accxavg + acczavg + Accvector + 
             Rheartrate + stress )
summary(model)


full.model <- lm(Rheartrate ~., data = subdata)

step1 <- stepAIC(full.model, direction="both")

attach(subdata)
model = lm(Rheartrate ~ Gender + Anti.depressants + Anxiolytics + Smoke + 
             Sleep + acczavg + hravg + trigger )
summary(model)

#incase tidysrse doesnt work --> khafam kard!
#remove.packages(c( "tidyverse", "plotly"))
#install.packages('tidyverse', dependencies = TRUE)
#install.packages('plotly', dependencies = TRUE)
subsetplot = subdata <-data[,c("Rheartrate","hravg","stress")]


#3D Plots
with(subdata, plot3d(Rheartrate,hravg,trigger))
with(subdata, plot3d(Rheartrate,hravg,trigger, type = 's', col=as.integer(Gender)+14))
with(subdata, plot3d(Rheartrate,hravg,trigger, type = 's', col=as.integer(Anxiolytics)+9))

#with(subdata, plot3d(Rheartrate,hravg,trigger, type = 'l', col=as.integer(Gender)+2))
#scatter3d(x = Rheartrate, y = hravg, z = trigger,surface=FALSE, grid = FALSE, ellipsoid = TRUE)

