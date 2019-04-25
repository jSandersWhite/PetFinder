petNum <- read.csv('train/train.csv')
petTest <- read.csv('test/test.csv')
petTest <- petTest[-c(2, 18, 19, 21, 22)]
## Remove non numeric data
## SVM model requires only numeric data, remove non-numeric vectors and factor the dependent variable
petNum <- petNum[-c(2, 18, 19, 21, 22)]
petNum$AdoptionSpeed <- factor(petNum$AdoptionSpeed, levels = c(0, 1, 2, 3, 4),
                               labels = c("Same Day", "7 Days", "30 Days", "90 Days", "No Adoption"))

## Test full SVM model
petSVM <- petNum
classifierSVM <- ksvm(petSVM$AdoptionSpeed ~ ., data = petSVM, kernel = "rbfdot")
predictSVM <- predict(classifierSVM, petTest)
summary(predictSVM)
predictSVM <- as.integer(predictSVM)
prop.table(table(predictSVM))
prop.table(table(petNum$AdoptionSpeed))

## Build factor as integer
petAdoptionInt <- as.integer(petNum$AdoptionSpeed)
petNum <- petNum[-19] ## remove DV to build predictor
## test same day only
petNumSameDay <- petNum
petNumSameDay$SameDay <- ifelse(petAdoptionInt == 1, 1, 0)
petNumSameDay$SameDay <- factor(petNumSameDay$SameDay, labels = c("Yes", "No"), levels = c(1, 0))
classSameDay <- ksvm(petNumSameDay$SameDay ~ ., data = petNumSameDay, kernel = "rbfdot")
predictSameDay <- predict(classSameDay, petTest)


## 1 week test
petNum1 <- petNum
petNum1$Adopt1 <- ifelse(petAdoptionInt == 2, 1, 0)
petNum1$Adopt1 <- factor(petNum1$Adopt1, labels = c("Yes", "No"), levels = c(1, 0))
classWeek <- ksvm(petNum1$Adopt1 ~ ., data = petNum1, kernel = "rbfdot")
predictWeek <- predict(classWeek, petTest)


## 30 day test
petNum2 <- petNum
petNum2$Adopt2 <- ifelse(petAdoptionInt == 3, 1, 0)
petNum2$Adopt2 <- factor(petNum2$Adopt2, levels = c(1, 0), labels = c("Yes", "No"))
class30Day <- ksvm(petNum2$Adopt2 ~ ., data = petNum2, kernel = "rbfdot")
predict30Day <- predict(class30Day, petTest)


## 90 day test
petNum3 <- petNum
petNum3$Adopt3 <- ifelse(petAdoptionInt == 4, 1, 0)
petNum3$Adopt3 <- factor(petNum3$Adopt3, levels = c(1, 0), labels = c("Yes", "No"))
class90Day <- ksvm(petNum3$Adopt3 ~ ., data = petNum3, kernel = "rbfdot")
predict90Day <- predict(class90Day, petTest)


## No Adoption Test
petNum4 <- petNum[-17]
petNum4$Adopt4 <- ifelse(petAdoptionInt == 5, 1, 0)
petNum4$Adopt4 <- factor(petNum4$Adopt4, levels = c(1,0), labels = c("Yes", "No"))
petNum4Train <- petNum4[1:11994, ]
petNum4Test <- petNum4[11995:14993, ]
classNoAdopt <- ksvm(petNum4Train$Adopt4 ~ ., data = petNum4Train, kernel = "rbfdot")
predictNoAdopt <- predict(classNoAdopt, petTest)
agmtNoAdopt <- predictNoAdopt == petNum4Test$Adopt4
table(agmtNoAdopt)
prop.table(table(agmtNoAdopt)) ## 99.67% 

petSVM <- petNum
classifierSVM <- ksvm(petSVM$AdoptionSpeed ~ ., data = petSVM, kernel = "rbfdot")
predictSVM <- predict(classifierSVM, petTest)
summary(predictSVM)
predictSVM <- as.integer(predictSVM)
prop.table(table(predictSVM))
prop.table(table(petNum$AdoptionSpeed))
