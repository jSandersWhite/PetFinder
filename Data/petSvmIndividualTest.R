## Create CSV of predictions by pet ID
## Read in raw data
petNum <- read.csv("train/train.csv")

## Remove non numeric data
## SVM model requires only numeric data, remove non-numeric vectors and factor the dependent variable
petNum2 <- petNum
petNum <- petNum[-c(2, 19, 21, 22)]
petNum$AdoptionSpeed <- factor(petNum$AdoptionSpeed, levels = c(0, 1, 2, 3, 4),
                               labels = c("Same Day", "7 Days", "30 Days", "90 Days", "No Adoption"))
## split data into 80/20 train/test sets
petNumTrain <- petNum[1:11994, ]
petNumTest <- petNum[11995:14993, ]

## load packages
library(kernlab)
library(caret)
library(gmodels)
## More complex kernel - Gaussian RBF
classifier <- ksvm(petNum$AdoptionSpeed ~ ., data = petNum, kernel = "rbfdot")
prediction <- predict(classifier, petNum)
agreement <- prediction == petNum$AdoptionSpeed
prop.table(table(agreement)) ## 44.4% true
svmPredictions <- data.frame("Pet ID" = petNum2[22], "SVM Prediction" = prediction) ## Create data frame of prediction by pet ID
saveRDS(svmPredictions, file = "svmPredictions.csv")


## Add more boolean variables
petNum$PhotoY <- ifelse(petNum$PhotoAmt > 0, 1, 0)
petNum$VideoY <- ifelse(petNum$VideoAmt > 0, 1, 0)
petNumTrain <- petNum[1:11994, ]
petNumTest <- petNum[11995:14993, ]

## test with new boolean vars and remove noise
class2 <- ksvm(petNumTrain$AdoptionSpeed ~ ., data = petNumTrain, kernel = "rbfdot")
prediction2 <- predict(class2, petNumTest)
agmt2 <- prediction2 == petNumTest$AdoptionSpeed
table(agmt2)
prop.table(table(agmt2))
CrossTable(prediction2, petNumTest$AdoptionSpeed)

## test same day only
petNumSameDay <- petNum[-17]
petAdoptionInt <- as.integer(petNum$AdoptionSpeed)
petNumSameDay$SameDay <- ifelse(petAdoptionInt == 1, 1, 0)
petNumSameDay$SameDay <- factor(petNumSameDay$SameDay, labels = c("Yes", "No"), levels = c(1, 0))
petNumSameDayTrain <- petNumSameDay[1:11994, ]
petNumSameDayTest <- petNumSameDay[11995:14993, ]
classSameDay <- ksvm(petNumSameDayTrain$SameDay ~ ., data = petNumSameDayTrain, kernel = "rbfdot")
predictSameDay <- predict(classSameDay, petNumSameDayTest)
agreementSameDay <- predictSameDay == petNumSameDayTest$SameDay
table(agreementSameDay) 
prop.table(table(agreementSameDay)) ## 97%

## 1 week test
petNum1 <- petNum[-17]
petNum1$Adopt1 <- ifelse(petAdoptionInt == 2, 1, 0)
petNum1$Adopt1 <- factor(petNum1$Adopt1, labels = c("Yes", "No"), levels = c(1, 0))
petNum1Train <- petNum1[1:11994, ]
petNum1Test <- petNum1[11995:14993, ]
classWeek <- ksvm(petNum1Train$Adopt1 ~ ., data = petNum1Train, kernel = "rbfdot")
predictWeek <- predict(classWeek, petNum14Test)
agmtWeek <- predictWeek == petNum1Test$Adopt1
table(agmtWeek)
prop.table(table(agmtWeek)) ## 78% true

## 30 day test
petNum2 <- petNum[-17]
petNum2$Adopt2 <- ifelse(petAdoptionInt == 3, 1, 0)
petNum2$Adopt2 <- factor(petNum2$Adopt2, levels = c(1, 0), labels = c("Yes", "No"))
petNum2Train <- petNum2 [1:11994, ]
petNum2Test <- petNum2[11995:14993, ]
class30Day <- ksvm(petNum2Train$Adopt2 ~ ., data = petNum2Train, kernel = "rbfdot")
predict30Day <- predict(class30Day, petNum2Test)
agmt30Day <- predict30Day == petNum2Test$Adopt2
table(agmt30Day)
prop.table(table(agmt30Day)) ## 73% 

## 90 day test
petNum3 <- petNum[-17]
petNum3$Adopt3 <- ifelse(petAdoptionInt == 4, 1, 0)
petNum3$Adopt3 <- factor(petNum3$Adopt3, levels = c(1, 0), labels = c("Yes", "No"))
petNum3Train <- petNum3[1:11994, ]
petNum3Test <- petNum3[11995:14993, ]
class90Day <- ksvm(petNum3Train$Adopt3 ~ ., data = petNum3Train, kernel = "rbfdot")
predict90Day <- predict(class90Day, petNum3Test)
agmt90Day <- predict90Day == petNum3Test$Adopt3
table(agmt90Day)
prop.table(table(agmt90Day)) ## 79%

## No Adoption Test
petNum4 <- petNum[-17]
petNum4$Adopt4 <- ifelse(petAdoptionInt == 5, 1, 0)
petNum4$Adopt4 <- factor(petNum4$Adopt4, levels = c(1,0), labels = c("Yes", "No"))
petNum4Train <- petNum4[1:11994, ]
petNum4Test <- petNum4[11995:14993, ]
classNoAdopt <- ksvm(petNum4Train$Adopt4 ~ ., data = petNum4Train, kernel = "rbfdot")
predictNoAdopt <- predict(classNoAdopt, petNum4Test)
agmtNoAdopt <- predictNoAdopt == petNum4Test$Adopt4
table(agmtNoAdopt)
prop.table(table(agmtNoAdopt)) ## 99.67% 

## 90 Day test, set seed
petNum3 <- petNum[-17]
petNum3$Adopt3 <- ifelse(petAdoptionInt == 4, 1, 0)
petNum3$Adopt3 <- factor(petNum3$Adopt3, levels = c(1, 0), labels = c("Yes", "No"))
set.seed(1812)
petNum3Train <- petNum3[1:12750, ]
petNum3Test <- petNum3[12751:14993, ]
petNum3Test <- petNum3Test[-20]
class90Day <- ksvm(petNum3Train$Adopt3 ~ ., data = petNum3Train, kernel = "rbfdot")
predict90Day <- predict(class90Day, petNum3Test)
agmt90Day <- predict90Day == petNum3Test$Adopt3
prop.table(table(agmt90Day))
