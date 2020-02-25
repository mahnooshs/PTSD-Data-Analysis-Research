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
#par(mfrow=c(3,3)) 

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
#kalman normalized hr
df1$knhr= range01(df1$hr1)
plot(df1$hr1)
points(match(df1$hr1[df1$ptsd_moment=='STRESSMOMENT'],df1$hr1),
       df1$hr1 [df1$ptsd_moment=='STRESSMOMENT'], col='red', pch = 19, cex=1.5)
#df1$hrn=standardize(df1$hr, centerFun = mean, scaleFun = sd)
theme_set(theme_bw())
#With GGPLOT
ggplot(data=df1) +geom_point(aes( x= seq(1, length(df1$nhr)), y=df1$nhr))+
  geom_point(aes(which (df1$ptsd_moment %in% 'STRESSMOMENT'),
                 df1$nhr [df1$ptsd_moment=='STRESSMOMENT']), col='red', size =3)



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
par(mfrow=c(1,2))



par(cex.main=0.75)

###detrended fluctuation
dfa(d4$hr, 
    main='Detrended Fluctuation Analysis for NON stress window')
dfa(df1$hr1,
    main='Detrended Fluctuation Analysis for stress window')

#In case you want the window size
l1 = dfa(d4$hr ,window.size.range=c(100,600),
    main='Detrended Fluctuation Analysis for NON stress window')
l2 = dfa(df1$hr1 ,window.size.range=c(100,600),
    main='Detrended Fluctuation Analysis for stress window')

library (plyr)

##Ploting DFA in ggplot
dfahealthy <- ldply (l1, data.frame)
names(dfahealthy)[names(dfahealthy) == ".id"] <- "type"
names(dfahealthy)[names(dfahealthy) == "X..i.."] <- "value"
X = subset(dfahealthy, dfahealthy$type == 'window.sizes')
names(X)[names(X) == "value"] <- "window.sizes"
names(X)[names(X) == "type"] <- "m"
Y = subset(dfahealthy, dfahealthy$type == 'fluctuation.function')
names(Y)[names(Y) == "value"] <- "fluctuation.function"
names(Y)[names(Y) == "type"] <- "s"
dfahealthy= data.frame(X,Y)
dfahealthy$type= 'Healthy'


dfaptsd <- ldply (l2, data.frame)
names(dfaptsd)[names(dfaptsd) == ".id"] <- "type"
names(dfaptsd)[names(dfaptsd) == "X..i.."] <- "value"
X = subset(dfaptsd, dfaptsd$type == 'window.sizes')
names(X)[names(X) == "value"] <- "window.sizes"
names(X)[names(X) == "type"] <- "m"
Y = subset(dfaptsd, dfaptsd$type == 'fluctuation.function')
names(Y)[names(Y) == "value"] <- "fluctuation.function"
names(Y)[names(Y) == "type"] <- "s"
dfaptsd= data.frame(X,Y)
dfaptsd$type= 'PTSD'

dfatotal = rbind(dfahealthy,dfaptsd)

ggplot(dfatotal)+geom_point(aes(x=dfatotal$window.sizes, 
                                  y = dfatotal$fluctuation.function, 
                                  color= dfatotal$type,shape=dfatotal$type,
                                size= dfatotal$type), size =5)+
  ggtitle('Fluctuation Graph')+ xlab("Window size: t") + 
  ylab("Fluctuation function: F(t)")+ labs(color='Type') +labs(shape="Type")+
  scale_color_manual(values = c("black", "black"))+
  scale_shape_manual(values=c(16,17))+
  theme(legend.text  = element_text(size=30))

ggsave('dfa.pdf',dpi=300)
ggsave('dfa.png',dpi=300)


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
) + ggtitle("Healthy")+ theme(
  plot.title = element_text(hjust = 0.5, size = 10))+
  theme(plot.title = element_text(size = 24))

#ggsave('Autocorrelation for PTSD windows.pdf')


plot2 = ggAcf(
  df1$hr1,
  lag.max = 450,
  type = c("correlation"),
  plot = TRUE,
  na.action = na.contiguous,
  demean = TRUE
) + ggtitle("PTSD ") +theme(
  plot.title = element_text(hjust = 0.5, size = 10))+
  theme(plot.title = element_text(size = 24))

#plot3=grid.arrange(plot1, plot2, ncol=2)
#plot3
library(ggpubr)
ggarrange(plot1,plot2)
ggsave('Autocorrelation for windows of heart rate.pdf', dpi=300)
ggsave('Autocorrelation for windows of heart rate.png', dpi=300)


##Ploting hr healthy window and hr nonhealthy

for (i in 1:600) {
  d4$Time[i] = i
}
d4$type= "Healthy"
d4$ptsd_moment=""
names(d4)[names(d4) == "nhr"] <- "knhr"

d5 = df1[c('hr','knhr', 'ptsd_moment')]
for (i in 1:600) {
  d5$Time[i] = i
}
d5$type="PTSD"
d6=rbind(d4,d5)
theme_set(theme_bw(base_size=24))
#With GGPLOT
ggplot(data = d6, aes(Time,knhr)) +
  geom_line(aes( x= Time, y=knhr), size=1.4)+
         ylab( "Normalized heart rate")+ xlab ("Time(s)") + facet_wrap(~ type)+
  geom_point(data= subset(d6,type=='PTSD'),
           aes (x= 100, y=0.1529442),
           color="red", size=4)

##Best wy to do it
ggplot(data = d6, aes(Time,knhr)) +
  geom_line(aes( x= Time, y=knhr), size=1.4)+
  ylab( "Normalized heart rate")+ xlab ("Time(s)") + facet_wrap(~ type)+
  geom_point(data= subset(d6,type=='PTSD'),
             aes (x= which (subset(d6,type=='PTSD')$ptsd_moment %in% 'STRESSMOMENT'), 
                  y=subset(d6,type=='PTSD')$knhr[subset(d6,type=='PTSD')$ptsd_moment=='STRESSMOMENT']),
             color="red", size=7)


ggsave('HealthyPTSD.pdf', dpi=300)
ggsave('HealthyPTSD.png', dpi=300)



##Visualization plotting multuple stress moments, for i=3,6,7,19,28,37,45,66,46
ts = data.frame()
j=0
i=3
a=(i-1)*600+1
b=i*600
s1 = total [a:b,]
j =j+1
##Plot Imputed data df1
s1$hr[s1$hr==0]<-NA
s1$hr1=na_kalman(s1$hr, model = "StructTS", smooth = TRUE, nit = -1, maxgap = 75) 
#kalman normalized hr
s1$knhr= range01(s1$hr1)
s1$nhr = range01(s1$hr)
s1 = s1[c('hr','nhr','hr1','knhr', 'ptsd_moment')]
for (i in 1:600) {
  s1$Time[i] = i
  s1$window= j
}
ts<- rbind (ts,s1)

#in case you wanna save
#write.csv(ts,"Desktop/PTSD/PTSD Data/visualization.csv", row.names = FALSE)

ts = read.csv("Desktop/PTSD/PTSD Data/PTSD Descriptive (for Descriptive paper)/visualization.csv")


plot(s1$knhr)
points(match(s1$hr1[s1$ptsd_moment=='STRESSMOMENT'],s1$hr1),
       s1$hr1 [s1$ptsd_moment=='STRESSMOMENT'], col='red', pch = 19, cex=1.5)
#df1$hrn=standardize(df1$hr, centerFun = mean, scaleFun = sd)

ggplot(data = ts, aes(Time,knhr)) +
  geom_line(aes( x= Time, y=knhr), size=1.4)+
  ylab( "Normalized heart rate")+ xlab ("Time(s)") + facet_wrap(~ window)+ 
geom_point(data= subset(ts,window==1),aes (x=101,  y=0.5416667), color="red", size=3)+
geom_point(data= subset(ts,window==1),aes (x=107,  y=0.7083333), color="red", size=3)+
  geom_point(data= subset(ts,window==1),aes (x=108,  y=0.7500000), color="red", size=3)+
  geom_point(data= subset(ts,window==1),aes (x=115,  y=0.9166667), color="red", size=3)+
  geom_point(data= subset(ts,window==2),aes (x=101,  y=0.24), color="red", size=3)+
  geom_point(data= subset(ts,window==3),aes (x=101,  y=0.8993700 ), color="red", size=3)+
  geom_point(data= subset(ts,window==3),aes (x=107,  y=0.8616338), color="red", size=3)+
  geom_point(data= subset(ts,window==4),aes (x=101,  y=0.08928571 ), color="red", size=3)+
  geom_point(data= subset(ts,window==5),aes (x=101,  y=1), color="red", size=3)+
  geom_point(data= subset(ts,window==6),aes (x=101,  y=0.1891892), color="red", size=3)+
  geom_point(data= subset(ts,window==7),aes (x=101,  y=0.1529442), color="red", size=3)+
  geom_point(data= subset(ts,window==8),aes (x=101,  y=0.5809663), color="red", size=3)+
  geom_point(data= subset(ts,window==8),aes (x=102,  y=0.6529577), color="red", size=3)+
  geom_point(data= subset(ts,window==9),aes (x=101,  y=0.6995142), color="red", size=3)+
  geom_point(data= subset(ts,window==9),aes (x=292,  y=0.6289503), color="red", size=3)+
  geom_point(data= subset(ts,window==9),aes (x=542,  y=0.4016297), color="red", size=3)
  
ggsave('Visualization.pdf',height = 14 , width = 11, dpi=300)
ggsave('Visualization.png',height = 14, width = 11, dpi=300) 
  
#x= which (subset(ts,window=='9')$ptsd_moment %in% 'STRESSMOMENT') 
#y=subset(ts,window=='9')$knhr[subset(ts,window=='9')$ptsd_moment=='STRESSMOMENT']


