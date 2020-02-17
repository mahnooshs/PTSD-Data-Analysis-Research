### Seperated windows of ptsd and non ptsd to check what differences are, features were extracted in python
ls()
rm(list=ls())


library(plot3D)
library(rgl)
library(car)
library(ggplot2)
library(dplyr)
library (cowplot)
library (normalr)
library(boot)
library (NormalizeMets)


data<-read.csv ("Desktop/PTSD/PTSD Data/All Features/All Features_dataframe.csv", header=TRUE)
data1<-data[data$ptsd_moment==1,]
data2<-data[data$ptsd_moment==0,]

set.seed(100)
data3=sample_n(data2, 512)

total = rbind(data1, data3)


plot(total$hrsd, total$hrrange, col=total$ptsd_moment)

ggplot2.scatterplot(data=data, xName= 'hrrange',yName='hrsd',
                    mapping=aes(size = qsec))


ggplot (total, aes (x = hrmean, y = hrsd, colour = ptsd_moment)) + stat_density2d ()
ggplot(total)+ geom_point(aes(x=hrmean,y=hrsd,colour=ptsd_moment))+theme_bw()

plot1 = ggplot(data1)+ geom_point(aes(x=hrrange,y=hrsd,colour=ptsd_moment))
plot2= ggplot(data2)+ geom_point(aes(x=hrrange,y=hrsd,colour=ptsd_moment))+ 
  scale_y_continuous(name="Stopping distance", limits=c(0, 25))
plot_grid(plot1, plot2, labels = "AUTO")


model1 <- glm(data1$hrsd ~ data1$hrrange, poisson)
model2 <- glm(data2$hrsd ~ data2$hrrange, poisson)
model3 <- glm(total$hrsd ~ total$hrrange+total$ptsd_moment, poisson)

summary(model1)
summary(model3)



#datan= normalise(data, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")


ggplot(total)+ geom_point(aes(x=hrmax,y=hrsd,colour=ptsd_moment))+theme_bw()
ggplot(total)+ geom_point(aes(x=hrmin,y=hrsd,colour=ptsd_moment))+theme_bw()
ggplot(total)+ geom_point(aes(x=hrmax,y=hrmin,colour=ptsd_moment))+theme_bw()
ggplot(total)+ geom_point(aes(x=hrmin,y=hrrange,colour=ptsd_moment))+theme_bw()
ggplot(total)+ geom_point(aes(x=(hrmax+hrmin)/2,y=hrrange,colour=ptsd_moment))+theme_bw()
ggplot(total)+ geom_point(aes(x=hrmean,y=hrsd,colour=ptsd_moment))+theme_bw()
ggplot(total)+ geom_point(aes(x=linaccmean,y=hrsd,colour=ptsd_moment))+theme_bw()
ggplot(total)+ geom_point(aes(x=linaccmean,y=hrrange,colour=ptsd_moment))+theme_bw()
ggplot(total)+ geom_point(aes(x=linaccmean,y=hrrange,colour=ptsd_moment))+theme_bw()
ggplot(total)+ geom_point(aes(x=linaccmean,y=hrrange,colour=ptsd_moment))+theme_bw()
ggplot(total)+ geom_point(aes(x=hrmean,y=hrmin,colour=ptsd_moment))+theme_bw()
scatter3d(x = total$hrsd, y =total$hrmean, z = total$ptsd_moment)
scatter3d(x = total$hrmean, y =total$hrmax, z = total$linaccmean, color= total$ptsd_moment)
