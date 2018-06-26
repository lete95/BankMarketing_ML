library(caret)
library(tidyr)
library(dplyr)

set.seed(2018)

## Regular

newData <- cbind(mca[["ind"]]$coord, data$subscription)
colnames(newData)[12] <- "subscription"
newData <- as.data.frame(newData)

newData$subscription <- newData$subscription - 1
table(newData$subscription)

oldData <- data
data <- newData
rm(newData)

data$subscription <- as.numeric(data$subscription)

sapply(data,class)

colnames(data) <- c("v1","v2","v3","v4","v5","v6",
                    "v7","v8","v9","v10","v11","Vsubscription")

data$Vsubscription[data$Vsubscription==1] <- "Y"
data$Vsubscription[data$Vsubscription==0] <- "N"

data$Vsubscription <- as.factor(data$Vsubscription)

sapply(data,class)

## Train-test the first model

control <- trainControl(method = "cv", 
                        number = 3,classProbs = TRUE,summaryFunction=twoClassSummary)

RFModel <- caret::train(Vsubscription ~ .,
                         data = dataTrain,
                         method = "rf",
                         metric = "ROC",
                         trControl = control)

print(RFModel)


## Under Sampling

finalUnder <- trainControl(method = "cv", 
                        number = 3,classProbs = TRUE,
                        summaryFunction=twoClassSummary,sampling="down")

RFModelUnder <- caret::train(Vsubscription ~ .,
                               data = dataTrain,
                               method = "rf",
                               metric="ROC",
                               trControl = finalUnder)

print(RFModelUnder)

## Over sampling

finalOver <- trainControl(method = "cv", 
                           number = 3,classProbs = TRUE,
                           summaryFunction=twoClassSummary,sampling="up")

RFModelOver <- caret::train(Vsubscription ~ .,
                             data = dataTrain,
                             method = "rf",
                             metric = "ROC",
                             trControl = finalOver)

print(RFModelOver)

## ROSE y SMOTE generate synthetic data to balance the prediction of subscription

finalROSE <- trainControl(method = "cv", 
                           number = 3,classProbs = TRUE,
                           summaryFunction=twoClassSummary,sampling="rose")

RFModelROSE <- caret::train(Vsubscription ~ .,
                            data = dataTrain,
                            method = "rf",
                            metric="ROC",
                            trControl = finalROSE)

print(RFModelROSE)

## With SMOTE

finalSMOTE <- trainControl(method = "cv", 
                           number = 3,classProbs = TRUE,
                           summaryFunction=twoClassSummary,sampling="smote")

RFModelSMOTE <- caret::train(Vsubscription ~ .,
                            data = dataTrain,
                            metric="ROC",
                            method = "rf",
                            trControl = finalSMOTE)

print(RFModelSMOTE)

## And now, compare all the approaches

rALLMETHODS <- resamples(list(NormalRF=RFModel,
                              UnderRF=RFModelUnder,
                              OverRF=RFModelOver,
                              RoseRF=RFModelROSE,
                              SmoteRF=RFModelSMOTE))
bwplot(rALLMETHODS,metric="ROC",main="AUROC of all Methods Compared")

## We will chose model performing better

rm(list=setdiff(ls(), c("data","mca","oldData")))