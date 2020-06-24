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
theme_set(theme_bw(base_size=24))
#read Data
#demo is demographic data
###Took out diabetes type 1 and 2 info for all since they were same as diabetes
data<-read.csv ("Desktop/PTSD/PTSD Data/New/Data demographics for SA DC GL/CombinedData.csv", header=TRUE)

data$Trigger = log(data$stress+1)
subdata <-data[,c("Gender", "Anti.depressants","Anxiolytics", "Smoke", "Age","Sleep","Rheartrate","hravg","Alcohol", "Exercise", "Glucocorticoids")]


d <- density(data$Rheartrate, na.rm = TRUE) # returns the density data 
plot(d, main="",
     xlab="Resting heart rate",
     ylab="Density",) # plots the results

hist(data$Rheartrate, main="",
     xlab="Resting heart Rate",
     ylab="Frequency",labels=TRUE) 

#trigger is log stress for poisson distribution
#subdata$trigger = log(subdata$stress+1)
corr = cor(subdata)

ggcorrplot(cor(subdata), type = "lower",
           outline.col = "white")

ggcorrplot(corr, type = "lower",
           lab = TRUE,   insig = "blank")

##IF You want t osave the plots
#############ggsave('Corrplot.pdf', dpi=300)
#############ggsave('Corrplot.png', dpi=300)

library(corrplot)
corrplot(corr, type = "upper", order = "hclust",
         col = c("black", "white"), bg = "lightblue")


##Testing some correlations 
cor.test(data$Rheartrate,data$hravg, method=c("pearson", "kendall", "spearman"))

cor.test(data$Rheartrate,data$Smoke, method=c("pearson", "kendall", "spearman"))


cor.test(data$Rheartrate,data$Anti.depressants, method=c("pearson", "kendall", "spearman"))

cor.test(data$Rheartrate,data$Age, method="pearson")

cor.test(data$Rheartrate,data$Sleep, method="pearson")
cor.test(data$Rheartrate,data$Gender, method="pearson")

#including ACC data here is not useful based on domain knowledge
#Compute missing value
#subdata = transform(subdata,  accxavg= ifelse(is.na(accxavg), mean(accxavg, na.rm=TRUE), accxavg))
#subdata = transform(subdata,  accyavg= ifelse(is.na(accyavg), mean(accyavg, na.rm=TRUE), accyavg))
#subdata = transform(subdata,  acczavg= ifelse(is.na(acczavg), mean(acczavg, na.rm=TRUE), acczavg))
#subdata = transform(subdata,  Accvector= ifelse(is.na(Accvector), mean(Accvector, na.rm=TRUE), Accvector))
##subdata = transform(subdata,  accxavgLin= ifelse(is.na(accxavgLin), mean(accxavgLin, na.rm=TRUE), accxavgLin))
##subdata = transform(subdata,  accyavgLin= ifelse(is.na(accyavgLin), mean(accyavgLin, na.rm=TRUE), accyavgLin))
##subdata = transform(subdata,  acczavgLin= ifelse(is.na(acczavgLin), mean(acczavgLin, na.rm=TRUE), acczavgLin))

# Fit the full model for average heart rate --> the model is not the one used in papers
full.model <- lm(hravg ~., data = subdata)

step1 <- stepAIC(full.model, direction="both")


attach(subdata)

model = lm(hravg ~ Anti.depressants +  Smoke+ 
             Rheartrate + Trigger )
summary(model)

###this is modeled on resting heart rate, the model is the one used in papers
full.model <- lm(Rheartrate ~., data = subdata)

step1 <- stepAIC(full.model, direction="both")


attach(subdata)
model = lm(Rheartrate ~ Gender + Anti.depressants + Anxiolytics + Smoke + 
             Sleep + hravg)
summary(model)
plot(model)
cor(Anti.depressants, hravg, method = "pearson")
cor(Gender, hravg, method = "pearson")





#checking for multicollinearity (VIF>10) --> not found
vif(model)


#incase tidysrse doesnt work --> khafam kard!
#remove.packages(c( "tidyverse", "plotly"))
#install.packages('tidyverse', dependencies = TRUE)
#install.packages('plotly', dependencies = TRUE)
subsetplot = subdata <-data[,c("Rheartrate","hravg","stress")]


#3D Plots
with(subdata, plot3d(Rheartrate,hravg,Trigger))
with(subdata, plot3d(Rheartrate,hravg,Trigger, type = 's', col=as.integer(Gender)+14))
with(subdata, plot3d(Rheartrate,hravg,Trigger, type = 's', col=as.integer(Anxiolytics)+9))

#with(subdata, plot3d(Rheartrate,hravg,trigger, type = 'l', col=as.integer(Gender)+2))
#scatter3d(x = Rheartrate, y = hravg, z = trigger,surface=FALSE, grid = FALSE, ellipsoid = TRUE)

fit <- lm(data$stress~data$Rheartrate)
summary(fit)



########## Sleep for more than 6 hours and less than 6 hours


subdata$Sleep [subdata$Sleep==2]=0
subdata$Sleep [subdata$Sleep==3]=0
subdata$Sleep [subdata$Sleep==4]=1


full.model <- lm(Rheartrate ~., data = subdata)

step1 <- stepAIC(full.model, direction="both")


attach(subdata)
model = lm(Rheartrate ~ Gender + Anti.depressants + Anxiolytics + Smoke + 
             Sleep + hravg)
summary(model)

corr = cor(subdata)

ggcorrplot(corr, type = "lower",
           lab = TRUE,   insig = "blank")

cor.test(subdata$Rheartrate,subdata$Sleep, method="pearson")







