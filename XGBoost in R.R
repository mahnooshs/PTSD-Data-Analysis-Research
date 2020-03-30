
ls()
rm(list=ls())
#read Data
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
library(xgboost)
library(Matrix)
library(readr)
library(stringr)
library(caret)
library(car)

train = read.csv ("Desktop/PTSD/PTSD Data/Training and Testing Data Sets/Training Data Set 75 percent upsampled.csv", header=TRUE)
test = read.csv ("Desktop/PTSD/PTSD Data/Training and Testing Data Sets/Testing Data Set 75 percent.csv", header=TRUE)


#bstSparse <- xgboost(data , label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
train_label = train [,'ptsd_moment']

trainvars = train[,1:28]

#trainmatrix = xgb.DMatrix(data=as.matrix(trainvars), label=train_label)

dtrain <- xgb.DMatrix(data=as.matrix(trainvars), label=train_label)

set.seed(100)

xgb <- xgboost(data = data.matrix(trainvars), 
               label = train_label, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)