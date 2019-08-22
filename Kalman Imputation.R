#This code imputes missing heart rate data with Kalman Filtering method


library(data.table)
library(imputeTS)
#df= fread('/Users/mahnooshsadeghi/Desktop/PTSD/PTSD Data/All Data Together/ca_2KJQ.csv')


### Took out the file ca_22GG and dc_22GC, dc_22GF dc_22GP and GL_23l3, gl_3HBN ta_6175 because of the number of missing values and obs
###Due to some reasons it cannot impute around 10 files. Those files are implemented separately
i<- 1
p<- list.files("/Users/mahnooshsadeghi/Desktop/PTSD/PTSD Data/All Data Together/", pattern="*.csv", full.names=TRUE)

setwd("/Users/mahnooshsadeghi/Desktop/PTSD/PTSD Data/Kalman Imputed Data")
for (i in 1:98) {
  #reading in data
  df<- fread(filename <- p[[i]])
  df$hr[df$hr==0]<-NA
  df$hr=na_kalman(df$hr, model = "StructTS", smooth = TRUE, nit = -1, maxgap = 5)
  N=sprintf('Participant.%d.csv', i)
  write.csv(df, file=N)
}
