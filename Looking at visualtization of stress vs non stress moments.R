


df<-read.csv ("Desktop/PTSD/PTSD Data/CLEANEDFive mins windows of stress moments after stress/CleanedStressnotall.csv", header=TRUE)

par(mfrow=c(2,2))
df1 = df [1:600,]
#plot(df1$hr)
df2= df [3000:3600,]
df3= df[5400:6000,]
df4 = df [17400:18000,]

df2$id = seq.int(nrow(df2))
plot(df2$hr)
plot(df1$hr)
plot(df3$hr)
plot(df4$hr)


data = read.csv("Desktop/PTSD/PTSD Data/Kalman Imputed Data/Participant.2.csv", header=TRUE)


#####3 NOPTSD moment
data1 = data[6300:7000,]
plot(data1$hr)



df2$id2=(df2$id)^2
model <- lm(df2$hr~ df2$id+ df2$id^2)
myPredict <- predict( model ) 

ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col=2, lwd=2 ) 
lines(df2$id^2,predicted.intervals[,3],col='green',lwd=3)

