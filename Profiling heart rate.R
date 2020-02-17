ls()
rm(list=ls())
#read Data

p<- list.files( pattern="*.csv", full.names=TRUE)
#df<-read.csv ("CleanedStressp10.csv", header=TRUE)
total = data.frame()

for (i in 1:16) {
  #reading in data
  data <- read.csv (filename <- p[[i]])
  total<- rbind (total,data)
}
par(mfrow=c(3,3)) 

library(forecast)
library(ggplot2)
library(gridExtra)
library(zoo)
library(data.table)
library(imputeTS)
library(BBmisc)
library(scales)
library(tseries)
library(urca)
library(nonlinearTseries)


#Package for stationarity.test
#library(aTSA)
#library(cran)
df1 = total [1800:2400,]
df1$id = seq.int(nrow(df1))
plot(df1$hr)
plot(decompose(df1$hr))
plot(forecast(df1$hr), h=5)
autoplot((df1$hr))


acf(df1$hr, na.action=na.pass, lag.max = 20)

stationary.test(df1$hr, method = "adf" , nlag = NULL,
                type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)


#plot(df1$hr, col = ifelse(df1$ptsd_moment== 'STRESSMOMENT','red','black'))


plot(df1$hr)
points(match(df1$hr[df1$ptsd_moment=='STRESSMOMENT'],df1$hr),
       df1$hr [df1$ptsd_moment=='STRESSMOMENT'], col='red', pch = 19, cex=1.5)


par(mfrow=c(3,3))

### in i ro bayad jagozari koni neveshte adadasho tu daftaret kuchik sefide
for (i in 45:53) {
  #reading in data
  a=(i-1)*600+1
  b=i*600
  df1 = total [a:b,]
  
  #Regular plots - Not normalized
  ## plot(df1$hr)
  ##points(which (df1$ptsd_moment %in% 'STRESSMOMENT'),
  ##     df1$hr [df1$ptsd_moment=='STRESSMOMENT'], col='red', pch = 19, cex=1.5)

  ##Normalize Plots
  range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))}
  df1$nhr = range01(df1$hr)
  plot(df1$nhr,xlab="Time (s)", ylab="Heart Rate bpm", main='Heart Rate Around PTSD moment')
  points(which (df1$ptsd_moment %in% 'STRESSMOMENT'),
         df1$nhr [df1$ptsd_moment=='STRESSMOMENT'], col='red', pch = 19, cex=1.5)
  #points(match(df1$ptsd_moment[df1$ptsd_moment=='STRESSMOMENT'],df1$ptsd_moment),
   #      df1$hr [df1$ptsd_moment=='STRESSMOMENT'], col='red', pch = 19, cex=1.5)
}

##Plot Imputed data df1
df1 $hr[df1 $hr==0]<-NA
df1 $hr1=na_kalman(df1 $hr, model = "StructTS", smooth = TRUE, nit = -1, maxgap = 600) 
plot(df1$hr1)
points(match(df1$hr1[df1$ptsd_moment=='STRESSMOMENT'],df1$hr1),
       df1$hr1 [df1$ptsd_moment=='STRESSMOMENT'], col='red', pch = 19, cex=1.5)
#df1$hrn=standardize(df1$hr, centerFun = mean, scaleFun = sd)

#With GGPLOT
ggplot(data=df1) +geom_point(aes( x= seq(1, length(df1$nhr)), y=df1$nhr))+
  geom_point(aes(which (df1$ptsd_moment %in% 'STRESSMOMENT'),
                 df1$nhr [df1$ptsd_moment=='STRESSMOMENT']), col='red', 
             size =3)



#imputed hr to check stationarity and stuff now name is hr1
#df1$hr[df1$hr==0]<-NA
#df1$hr1=na_kalman(df1$hr, model = "StructTS", smooth = TRUE, nit = -1, maxgap = 400)
##df1$nhr= normalize(df1$hr1, method = "scale", range = c(0, 1))
##df1$nhr = scale(df1$hr1, center = TRUE, scale = TRUE)
##Normalizing data by defining a function
#range01 <- function(x){(x-min(x))/(max(x)-min(x))}
#df1$nhr = range01(df1$hr1)

###regular plot
plot(df1$hr1,main="Heart Rate Profle ine in a 600 seconds window of time with PTSD trigger", 
     xlab="Time (s)", ylab="Heart Rate bpm")
points(match(df1$hr1[df1$ptsd_moment=='STRESSMOMENT'],df1$hr1),
       df1$hr1 [df1$ptsd_moment=='STRESSMOMENT'], col='red', pch = 19, cex=1.5)

##Normalize Plot
plot(df1$nhr,main="Heart Rate Profle ine in a 600 seconds window of time with PTSD trigger", 
     xlab="Time (s)", ylab="Heart Rate bpm")
points(match(df1$nhr[df1$ptsd_moment=='STRESSMOMENT'],df1$nhr),
       df1$nhr [df1$ptsd_moment=='STRESSMOMENT'], col='red', pch = 19, cex=1.5)

#autocorrelation with original obs
acf(df1$hr, na.action=na.pass, lag.max = 60, main= 'Autocorrelation for stress windows')
#autocorrelation with imputed obs
acf(df1$hr1, na.action=na.pass, lag.max = 60, main= 'Autocorrelation for stress windows')
acf(df1$hr1, na.action=na.pass, lag.max = 20)
pacf(df1$hr, na.action=na.pass, lag.max = 20, main = 'windows without stress')

#you can check for stationarity by this function
adf.test(df1$hr1, k=0)

tsdisplay(df1$hr1)
auto.arima(df1$hr)

##NN PTSD
d = read.csv ("Documents/GitHub/PTSD-Data-Analysis-Research/Windowed Participant nonstress moments6.csv", header=TRUE)

#d1 = read.csv ("Desktop/PTSD/PTSD Data/Windowed Data/Windowed Participant1.csv", header=TRUE)

newdata <- subset(d, windowno== 906)
newdata $hr[newdata $hr==0]<-NA
newdata $hr1=na_kalman(newdata $hr, model = "StructTS", smooth = TRUE, nit = -1, maxgap = 600) 
plot(newdata$hr1)

                 
auto.arima(newdata$hr)
plot(newdata$hr)
acf(newdata$hr1,na.action=na.pass, lag.max = 60)
pacf(newdata$hr,na.action=na.pass, lag.max = 20)


adf.test(newdata$hr1)
ur.df(newdata$hr)



#online data
d3 = read.csv ("Desktop/hr2.csv", header=TRUE)

d4<- matrix(0, ncol = 1, nrow = 900)
d4 <- data.frame(d4)
names(d4)[names(d4) == "d4"] <- "hr"

j=1
for (i in 1:900){
  d4$hr[[i]] = (d3$hr[[j]]+d3$hr[[j+1]])/2
  j=j+2
}

d4= d4[1:600,]
d4 = data.frame(d4)
names(d4)[names(d4) == "d4"] <- "hr"
acf(d4$hr,na.action=na.pass, lag.max = 60, main= 'Autocorrelation for NON stress windows')
adf.test(d4$hr)
pacf(d4$hr, na.action=na.pass, lag.max = 20, main = 'windows without stress')



#non normalized plot
plot(d4$hr, main="Heart Rate Profline in a 600 seconds window of time", 
     xlab="Time (s)", ylab="Heart Rate bpm")



#normalized plot
d4$nhr = range01(d4$hr)
plot(d4$nhr, main="Healthy Heart Rate Profile", 
     xlab="Time (s)", ylab="Heart Rate bpm")


adf.test(d4$hr, k=0)
auto.arima(d4$hr)
kpss.test(d4$hr)
par(mfrow=c(1,1))




###detrended fluctuation
dfa(df1$hr1,window.size.range=c(1,150), main='Detrended Fluctuation Analysis for stress window')
dfa(d4$hr,window.size.range=c(1,150), main='Detrended Fluctuation Analysis for NON stress window')








###For myself for checking Dickey-Fullers test for random sample of stress moments
for (i in 1:10) {
  #reading in data
  a=(i-1)*600+1
  b=i*600
  df1 = total [a:b,]
  df1$hr[df1$hr==0]<-NA
  df1$hr1=na_kalman(df1$hr, model = "StructTS", smooth = TRUE, nit = -1, maxgap = 400)
  adf.test(df1$hr1, k=0)
}


###



plot1 = ggAcf(
  d4$hr,
  lag.max =450,
  type = c("correlation"),
  plot = TRUE,
  na.action = na.contiguous,
  demean = TRUE
) + ggtitle("Autocorrelation Plot for PTSD Windows ")+ theme(
  plot.title = element_text(hjust = 0.5, size = 10))

#ggsave('Autocorrelation for PTSD windows.pdf')


plot2 = ggAcf(
  df1$hr1,
  lag.max = 450,
  type = c("correlation"),
  plot = TRUE,
  na.action = na.contiguous,
  demean = TRUE
) + ggtitle("Autocorrelation Plot for PTSD Windows ") +theme(
  plot.title = element_text(hjust = 0.5, size = 10))

grid.arrange(plot1, plot2, ncol=2)

ncol()ggsave('Autocorrelation for windows of heart rate.pdf')


