
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
library(ggplot2)
library(vip)

training_set = read.csv ("Desktop/PTSD/PTSD Data/Training and Testing Data Sets/Training Data Set (without acc) 75 percent.csv", header=TRUE)
test_set = read.csv ("Desktop/PTSD/PTSD Data/Training and Testing Data Sets/Testing Data Set (without acc) 75 percent.csv", header=TRUE)

training_set = subset(training_set, select = -c(linear_accel_x,linear_accel_y,linear_accel_z) )
test_set = subset(test_set, select = -c(linear_accel_x,linear_accel_y,linear_accel_z) )

#trainmatrix = xgb.DMatrix(data=as.matrix(trainvars), label=train_label)

classifier = xgboost(data=as.matrix(training_set[-11]), 
                     label=training_set$ptsd_moment, objective="binary:logistic", nrounds = 10)

#classifier2 = xgboost::xgboost(data=as.matrix(training_set[-11]), 
         #                      label=training_set$ptsd_moment, nrounds = 10)
#clf1 = xgb.Booster.complete(classifier)

#Applying Kfold Cross validation
library (caret)
folds=createFolds(training_set$ptsd_moment, k=10)
CV= lapply(folds, function(x) {
  training_fold = training_set[-x,]
  test_fold= training_set[x,]
  classifier = xgboost(data=as.matrix(training_set[-11]), 
                       label=training_set$ptsd_moment, objective="binary:logistic", nrounds = 10)
  y_pred = predict(classifier, newdata= as.matrix(test_fold[-11]))
  y_pred=(y_pred >= 0.5)
  cm= table(test_fold[,11], y_pred)
  accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  return(accuracy)
})

accuracy=mean(as.numeric(CV))

##basic validation on testing dataset
y_pred = predict(classifier, newdata=as.matrix(test_set[-11]))
y_pred1=(y_pred >= 0.5)
cm= table (test_set[,11], y_pred1)


##ROC graphs

library (pROC)

library(ROCR)

pred = prediction(y_pred,test_set$ptsd_moment)
#eval = performance(pred, "hrmean")
#plot(eval)

roc = performance(pred, "tpr", "fpr")
plot(roc, colorize=T)
abline(a=0,b=1)


#Area under Curve
auc = performance(pred, "auc")
auc = unlist(slot(auc, "y.values"))
auc = round(auc,2)
legend(0.7,.5,auc,title = 'AUC')



##ggplots
#to draw this you need your roc function

#####Feature importance


# Same thing with co-occurence computation this time
library(Ckmeans.1d.dp)
xgb.importance(model = classifier)
xgb.plot.importance (importance_matrix = xgb.importance(model = classifier))
vip(classifier, num_features = 10)  

x <- data.matrix(subset(training_set, select = -ptsd_moment)) 


library(SHAPforxgboost)

shap_values <- shap.values(xgb_model = classifier, X_train = x)
shap_values$mean_shap_score
shap_values_data <- shap_values$shap_score
shap_long_data <- shap.prep(xgb_model = classifier, X_train = x)
#shap_long_iris <- shap.prep(shap_contrib = shap_values_iris, X_train = X1)
shap.plot.summary(shap_long_data, x_bound  = 4, dilute = 10)

shap.plot.dependence(data_long = shap_long_data, x="hrmean",
                     y = "hrmean", color_feature = "hrmean")
shap.plot.dependence(data_long = shap_long_data, x="linear_accel_z",
                     y = "linear_accel_z", color_feature = "linear_accel_z")
shap.plot.dependence(data_long = shap_long_data, x="linaccmean",
                     y = "linaccmean", color_feature = "linaccmean")
shap.plot.dependence(data_long = shap_long_data, x="hrsd",
                     y = "hrsd", color_feature = "hrsd")

shap_int <- predict(mod1, as.matrix(iris[,-5]),
                    predinteraction = TRUE)




shap.plot.dependence(data_long = shap_long_data, x="hrmin",
                     y = "hrmin", color_feature = "hrmin")


shap.plot.summary.wrap1(classifier, x, top_n = 10)


sapply(training_set, sd, na.rm = TRUE)

xgb.plot.shap(x, model = classifier, features='linaccmin')
shap.plot.dependence(data_long = shap_long_data, x="linaccmin",
                     y = "linaccmin", color_feature = "linaccmin")

shap_int <- predict(classifier, as.matrix(training_set[-11]),
                    predinteraction = TRUE)
shap.plot.dependence(data_long = shap_long_data,
                     data_int = shap_int,
                     x="linaccmean",
                     y = "hrmin",
                     color_feature = "hrmin")
