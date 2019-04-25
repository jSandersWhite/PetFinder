## Best models and predicted outputs for Tuned C.50, Bagging, and SVM 
## Import packages
library(kernlab)
library(caret)
library(gmodels)
library(ipred)
library(e1071)
library(adabag)

## Import data
pet    <- readRDS('data/pet.csv')
## Numerical data for SVM
petNum <- read.csv('data/train/train.csv')

## Clean the data
petNum$AdoptionSpeed <- factor(petNum$AdoptionSpeed, levels = c(0, 1, 2, 3, 4),
                               labels = c("Same Day", "7 Days", "30 Days", "90 Days", "No Adoption"))
## Remove petID, remove created vars
petID  <- pet[22]
petNum <- petNum[-c(2, 19, 21, 22)]
pet    <- pet[-c(2, 19, 21, 22)]
pet    <- pet[-c(24, 23, 22, 21)]

## Split into train/test sets
set.seed(1812)
petNumTrain <- petNum[1:12750, ]
petNumTest  <- petNum[12751:14993, ]
petTrain    <- pet[1:12751, ]
petTest     <- pet[12751:14993, ]
petIDTest   <- petID[12751:14993, ]

## Gaussian kernel SVM
classifierSVM <- ksvm(petNumTrain$AdoptionSpeed ~ ., data = petNumTrain, kernel = "rbfdot")
petPredictSVM <- predict(classifierSVM, petNumTest)
agreementSVM  <- petPredictSVM == petNumTest$AdoptionSpeed
CrossTable(petPredictSVM, petNumTest$AdoptionSpeed)
table(agreementSVM)
prop.table(table(agreementSVM))

## Save SVM predictions as RDS
svmPrediction <- data.frame(petID = petIDTest, prediction = petPredictSVM)
saveRDS(svmPrediction, file = 'svmPrediction.csv')


## Full tuned C5.0 model
ctrl          <- trainControl(method = "cv", selectionFunction = "oneSE")
grid          <- expand.grid(.model = "rules",
                             .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                             .winnow = "FALSE")
fullC5        <- train(AdoptionSpeed ~ ., data = petNumTrain, method = "C5.0",
                       metric = "Kappa",
                       trControl = ctrl,
                       tuneGrid = grid)
fullC5Predict <- predict(fullC5, petNumTest)
table(fullC5Predict, petNumTest$AdoptionSpeed)
head(predict(fullC5, petNumTest, type="prob"))
head(fullC5)

## Save tuned C5.0 model as RDS
tunedC5Prediction <- data.frame(petID = petIDTest, prediction = fullC5Predict)
saveRDS(tunedC5Prediction, file = 'fullc5Prediction.csv')


## Bootstrap aggregator model
fullbag         <- bagging(AdoptionSpeed ~ ., data = petTrain, nbagg = 25)
fullbagPred     <- predict(fullbag, petTest)
fullbagPred$confusion
fullbagCtrl     <- trainControl(method = "cv")
train(AdoptionSpeed ~ ., data = petTrain, method = "treebag", trControl = fullbagCtrl)
fullBagTestPred <- predict(fullbag, petTest)
fullbagTestPred$confusion
head(fullBagTestPred) ## Test set predictions from boostrap aggregator model

## Save single RDS
joshRDS <- data.frame(petID = petIDTest, svmPrediction = petPredictSVM, tunedC5Prediction = fullC5Predict, bagPredict = fullBagTestPred)
saveRDS(joshRDS, "josh.RDS")
