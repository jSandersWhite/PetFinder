## C5.0 model of pet data from petData.R
library(C50)
## Duplicate data
pet2 <- pet

## Remove unnecessary columns from data
names(pet2)
pet2 <- pet2[-c(2, 19, 21, 22, 25)]
names(pet2)

## Split the training data 99/1
## Make a random vector of integers
set.seed(475)
ninetyPct <- sample(14993, 13500)
str(ninetyPct)

## Split the set using the random vector
train <- pet2[ninetyPct, ]
test <- pet2[-ninetyPct, ]

## Check distribution of train and test data
prop.table(table(train$AdoptionSpeed))
prop.table(table(test$AdoptionSpeed))
summary(train)
## Build C5.0 model
adoptModel <- C5.0(train[-20], train$AdoptionSpeed, trials=10)
adoptModel
summary(adoptModel)
## Not much good data from the above
## Need to figure out which variables matter