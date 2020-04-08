
ls()
rm(list=ls())
#read Data

library(randomForest)
library(Matrix)
library(readr)
library(stringr)
library(caret)
library(car)
library(ggplot2)
library(vip)
set.seed(79)
training_set = read.csv ("Desktop/PTSD/PTSD Data/Training and Testing Data Sets/Training Data Set 75 percent upsampled.csv", header=TRUE)
test_set = read.csv ("Desktop/PTSD/PTSD Data/Training and Testing Data Sets/Testing Data Set 75 percent.csv", header=TRUE)
#training_set$ptsd_moment=as.factor(training_set$ptsd_moment)
classifier = randomForest(x = training_set[-29],
                          y= training_set$ptsd_moment,
                          ntree=10)
#Applying Kfold Cross validation
library (caret)
folds=createFolds(training_set$ptsd_moment, k=10)
CV= lapply(folds, function(x) {
  training_fold = training_set[-x,]
  test_fold= training_set[x,]
  classifier = randomForest(x = training_set[-29],
                            y= training_set$ptsd_moment,
                            ntree=10)
  y_pred = predict(classifier, newdata= as.matrix(test_fold[-29]))
  y_pred=(y_pred >= 0.5)
  cm= table(test_fold[,29], y_pred)
  accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  return(accuracy)
})

accuracy=mean(as.numeric(CV))

##basic validation on testing dataset
y_pred = predict(classifier, newdata=as.matrix(test_set[-29]))
y_pred1=(y_pred >= 0.5)
cm= table (test_set[,29], y_pred1)
cm





##AUC ROC
library (pROC)
library(ROCR)

pred = prediction(y_pred,test_set$ptsd_moment)
eval = performance(pred, "acc")
plot(eval)

roc = performance(pred, "tpr", "fpr")
plot(roc, colorize=T)
abline(a=0,b=1)


#Area under Curve
auc = performance(pred, "auc")
auc = unlist(slot(auc, "y.values"))
auc = round(auc,2)
legend(0.7,.5,auc,title = 'AUC')


# Same thing with co-occurence computation this time
library(Ckmeans.1d.dp)
vip(classifier, num_features = 10)  

#####pdp
library(pdp)
#classifier00 = xgboost(data=as.matrix(training_set[-29]),label=training_set$ptsd_moment,
#nrounds = 10, objective="binary:logistic")


x <- data.matrix(subset(training_set, select = -ptsd_moment)) 
#partial 1
partial(classifier, pred.var = "hrmin", plot.engine = "ggplot2", 
        train = x, plot=TRUE)

#partial2
partial(classifier, pred.var = "acc_zmax", ice = TRUE, center = TRUE, 
        plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "ggplot2",
        train = x)

#partial2
partial(classifier, pred.var = "hrmin", ice = TRUE, center = TRUE, 
        plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "ggplot2",
        train = x)


