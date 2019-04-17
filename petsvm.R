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

## Build the model
## linear kernel
classifier <- ksvm(petNumTrain$AdoptionSpeed ~ ., data = petNumTrain, kernel = "vanilladot")
classifier ## Basic information about the training params and fit of the model

## Test the model
petPredict <- predict(classifier, petNumTest)
head(petPredict)
table(petPredict, petNumTest$AdoptionSpeed)
agreement <- petPredict == petNumTest$AdoptionSpeed
table(agreement)
prop.table(table(agreement))## 33.4% accuracy 1997 FALSE 1002 TRUE

## Base model with all variables included not very accurate

## Improving the model
## More complex kernel - Gaussian RBF
classifierRBF <- ksvm(petNumTrain$AdoptionSpeed ~ ., data = petNumTrain, kernel = "rbfdot")
petPredictRBF <- predict(classifierRBF, petNumTest)
agreementRBF <- petPredictRBF == petNumTest$AdoptionSpeed
CrossTable(petPredictRBF, petNumTest$AdoptionSpeed)
table(agreementRBF)
prop.table(table(agreementRBF)) ## 37.8% accuracy

## Removing obvious noise variables
## Breed2, Color2, Color3
petNum2 <- petNum[-c(4, 7, 8)]
petNumTrain2 <- petNum2[1:11994, ]
petNumTest2 <- petNum2[11995:14993, ]
classifier2 <- ksvm(petNumTrain2$AdoptionSpeed ~ ., data = petNumTrain2, kernel = "rbfdot")
petPredict2 <- predict(classifier2, petNumTest2)
agreement2 <- petPredict2 == petNumTest2$AdoptionSpeed
table(agreement2)
prop.table(table(agreement2)) ## 37.2% accuracy
## No more accurate than general RBF test

## Age, quantity, PhotoAmt
petNum3 <- petNum2[-c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15)]
petNumTrain3 <- petNum3[1:11994, ]
petNumTest3 <- petNum3[11995:14993, ]
classifier3 <- ksvm(petNumTrain3$AdoptionSpeed ~ ., data = petNumTrain3, kernel = "rbfdot")
petPredict3 <- predict(classifier3, petNumTest3)
agreement3 <- petPredict3 == petNumTest3$AdoptionSpeed
table(agreement3)
prop.table(table(agreement3))
## 35.5% accuracy

## Above test using polynomial kernel
petNumPoly1 <- petNum3
petNumPolyTrain1 <- petNumPoly1[1:11994, ]
petNumPolyTest1 <- petNumPoly1[11995:14993, ]
polyClassifier1 <- ksvm(petNumPolyTrain1$AdoptionSpeed ~ ., data = petNumPolyTrain1, kernel = "polydot")
polyPredict1 <- predict(polyClassifier1, petNumPolyTest1)
polyAgreement1 <- polyPredict1 == petNumPolyTest1$AdoptionSpeed
table(polyAgreement1)
prop.table(table(polyAgreement1))
## 33.5%

## base model with different kernels
## Polynomial
petNumPoly2 <- petNum
petNumPolyTrain2 <- petNumPoly2[1:11994, ]
petNumPolyTest2 <- petNumPoly2[11995:14993, ]
polyClass2 <- ksvm(petNumPolyTrain2$AdoptionSpeed ~ ., data = petNumPolyTrain2, kernel = "polydot")
polyPredict2 <- predict(polyClass2, petNumPolyTest2)
polyAgmt2 <- polyPredict2 == petNumPolyTest2$AdoptionSpeed
table(polyAgmt2)
prop.table(table(polyAgmt2))
## 33.4%

## Hyperbolic tangent
petNumTanh <- petNum
petNumTanhTrain <- petNumTanh[1:11994, ]
petNumTanhTest <- petNumTanh[11995:14993, ]
tanhClass <- ksvm(petNumTanhTrain$AdoptionSpeed ~ ., data = petNumTanhTrain, kernel = "tanhdot")
tanhPred <- predict(tanhClass, petNumTanhTest)
tanhAgmt <- tanhPred == petNumTanhTest$AdoptionSpeed
table(tanhAgmt)
prop.table(table(tanhAgmt))
## 24% 

## Remove noise vars
petNum <- petNum[-c(4, 7, 8)]
petNumTrain <- petNum[1:11994, ]
petNumTest <- petNum[11995:14993, ]

## Laplacian kernel
petLaplace <- petNum
petLaplaceTrain <- petNumTrain
petLaplaceTest <- petNumTest
lapClass <- ksvm(petLaplaceTrain$AdoptionSpeed ~ ., data = petLaplaceTrain, kernel = "laplacedot")
lapPred <- predict(lapClass, petLaplaceTest)
lapAgmt <- lapPred == petLaplaceTest$AdoptionSpeed
table(lapAgmt)
prop.table(table(lapAgmt))
## 36.9%

## Bessel kernel
besselClass <- ksvm(petNumTrain$AdoptionSpeed ~ ., data = petNumTrain, kernel = "besseldot")
besselPred <- predict(besselClass, petNumTest)
besselAgmt <- besselPred == petNumTest$AdoptionSpeed
table(besselAgmt)
prop.table(table(besselAgmt))
## 27.9%

## ANOVA RBF Kernel
anovaClass <- ksvm(petNumTrain$AdoptionSpeed ~ ., data = petNumTrain, kernel = "anovadot")
anovaPred <- predict(anovaClass, petNumTest)
anovaAgmt <- anovaPred == petNumTest$AdoptionSpeed
table(anovaAgmt)
prop.table(table(anovaAgmt))
## 

## More Gaussian tests
## Remove other potential noise variables
gaussian2 <- petNum[-c(1, 3, 4, 14)]
gaussian2Train <- gaussian2[1:11994, ]
gaussian2Test <- gaussian2[11995:14993, ]
gaussian2Class <- ksvm(gaussian2Train$AdoptionSpeed ~ ., data = gaussian2Train, kernel = "rbfdot")
gaussian2Pred <- predict(gaussian2Class, gaussian2Test)
gaussian2Agmt <- gaussian2Pred == gaussian2Test$AdoptionSpeed
table(gaussian2Agmt)
CrossTable(gaussian2Pred, gaussian2Test$AdoptionSpeed)
prop.table(table(gaussian2Agmt))
## 37.1%

## Improving the model
library(ipred)
set.seed(1812)
mybag <- bagging(AdoptionSpeed ~ ., data = petNum, nbagg = 25)
mybagPred <- predict(mybag, petNum)
mybagPred$confusion
library(e1071)
ctrl <- trainControl(method = "cv", selectionFunction = "oneSE")
bagctrl <- bagControl(fit = svmBag$fit,
                      predict = svmBag$pred,
                      aggregate = svmBag$aggregate)
svmbag <- train(AdoptionSpeed ~ ., data = petNum, "bag",
                trControl = ctrl, bagControl = bagctrl)
library(adabag)
m_adaboost<-boosting(AdoptionSpeed ~ ., data = petNum)
predAdaboost <- predict(m_adaboost, petNum)
head(predAdaboost$class)
predAdaboost$confusion
