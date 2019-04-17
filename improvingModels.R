## Read in raw data
petNum <- read.csv("train/train.csv")

## Remove non numeric data
## SVM model requires only numeric data, remove non-numeric vectors and factor the dependent variable
petNum2 <- petNum
petNum <- petNum[-c(2, 19, 21, 22)]
petNum$AdoptionSpeed <- factor(petNum$AdoptionSpeed, levels = c(0, 1, 2, 3, 4),
                               labels = c("Same Day", "7 Days", "30 Days", "90 Days", "No Adoption"))
## split data into 80/20 train/test sets
set.seed(1812)
petNumTrain <- petNum[1:11994, ]
petNumTest <- petNum[11995:14993, ]

## load packages
library(kernlab)
library(caret)
library(gmodels)
library(ipred)
library(e1071)
library(adabag)

## Create small sample model to run tests on
set.seed(1812)
petNumSamp <- petNum[1:750, ]

## Simple C5.0 tuned model
set.seed(1812)
m <- train(AdoptionSpeed ~ ., data = petNumSamp, method = "C5.0" )
p <- predict(m, petNumSamp)
table(p, petNumSamp$AdoptionSpeed)
head(predict(m, petNumSamp, type="prob"))
ctrl <- trainControl(method = "cv", selectionFunction = "oneSE")
## Set rules for model to improve performance
grid <- expand.grid(.model = "rules",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")
m <- train(AdoptionSpeed ~ ., data = petNumSamp, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
p <- predict(m, petNumSamp)
table(p, petNumSamp$AdoptionSpeed)
head(predict(m, petNumSamp, type="prob"))
## Best model - 25 trials, 38.7% accuracy, Kappa 0.1793

## Bootstrap aggregation
mybag <- bagging(AdoptionSpeed ~ ., data = petNumSamp, nbagg = 25)
mybagPred <- predict(mybag, petNumSamp)
mybagPred$confusion
bagCtrl <- trainControl(method = "cv")
train(AdopionSpeed ~ ., data = petNumSamp, method = "treebag", trControl = bagCtrl)
## Accuracy - 37.2%
## Kappa - 0.1568

## SVM Bag
bagctrl <- bagControl(fit = svmBag$fit,
                         predict = svmBag$pred,
                         aggregate = svmBag$aggregate)
svmbag <- train(AdoptionSpeed ~ ., data = petNumSamp, "bag",
                trControl = ctrl, bagControl = bagctrl)
svmbag

## AdaBoost.M1
m_adaboost <- boosting(AdoptionSpeed ~ ., data = petNumSamp)
p_adaboost <- predict(m_adaboost, petNumSamp)
p_adaboost$confusion
adaboost_cv <- boosting.cv(AdoptionSpeed ~ ., data = petNumSamp)
adaboost_cv$confusion
library(vcd)
Kappa(adaboost_cv$confusion)

## Full C5.0 model
mFull <- train(AdoptionSpeed ~ ., data = petNum, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
pFull <- predict(mFull, petTest)
table(pFull, petNum$AdoptionSpeed)
head(predict(mFull, petNum, type="prob"))
mFull
petTest <- read.csv("test/test.csv")
petTest <- petTest[-c(2, 19, 21, 22)]
head(pFull) ## Test predictions from C.50 Tuned Model

## Full bag
fullbag <- bagging(AdoptionSpeed ~ ., data = petNum, nbagg = 25)
fullbagPred <- predict(fullbag, petNum)
fullbagPred$confusion
fullbagCtrl <- trainControl(method = "cv")
train(AdoptionSpeed ~ ., data = petNum, method = "treebag", trControl = fullbagCtrl) ## Accuracy 38.8% Kappa 0.187
fullBagTestPred <- predict(fullbag, petTest)
fullbagPred$confusion
head(fullBagTestPred) ## Test set predictions from boostrap aggregator model
