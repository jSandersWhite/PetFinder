## Pet adoption prediction, attempting to accrately predict AdoptionSpeed values
## 
## Data and factor labels come from Kaggle project at https://www.kaggle.com/c/petfinder-adoption-prediction/data
##
## Read in training data - Assumption is project is in home directory of all data
## 
## Some missing values in Breed 1, many in Breed 2
pet <- read.csv("train/train.csv", stringsAsFactors = F)
petBreeds <- read.csv("breed_labels.csv")
petColors <- read.csv("color_labels.csv")
stateId <- read.csv("state_labels.csv")
## Used libraries
library(gmodels)
library(dplyr)

## Sets factor levels and labels on the appropriate data
pet$Type <- factor(pet$Type, levels = c(1, 2), labels = c("Dog", "Cat"))
table(pet$Type) ## data check
pet$Gender <- factor(pet$Gender, levels =c(1, 2, 3), labels = c("Male", "Female", "Mixed'"))
table(pet$Gender) ## Data check
pet$Vaccinated <- factor(pet$Vaccinated, levels = c(1,2,3), labels = c("Yes", "No", "Not Sure"))
pet$Dewormed <- factor(pet$Dewormed, levels = c(1,2,3), labels = c("Yes", "No", "Not Sure"))
pet$Sterilized <- factor(pet$Sterilized, levels = c(1, 2, 3), labels = c("Yes", "No", "Not Sure"))
pet$Health <- factor(pet$Health, levels = c(1, 2, 3, 0), 
                     labels = c("Healthy", "Minor Injury", "Major Injury", "Not Specified"))
pet$MaturitySize <- factor(pet$MaturitySize, levels = c(1, 2, 3, 4, 0), 
                           labels = c("Small", "Medium", "Large", "Extra Large", "Not Specified"))
pet$FurLength <- factor(pet$FurLength, levels = c(1, 2, 3, 0), 
                        labels = c("Short", "Medium", "Long", "Not Specified"))
## Assigns breed labels as factor from breed_labels.csv
## May turn 0 into a factor, 0 is currently N/A
pet$Breed1 <- factor(pet$Breed1, levels = petBreeds$BreedID, labels = petBreeds$BreedName)
pet$Breed2 <- factor(pet$Breed2, levels = petBreeds$BreedID, labels = petBreeds$BreedName)
## Assigns color labels as factor from color_labels.csv
## May turn 0 into a factor, 0 is currently N/A
pet$Color1 <- factor(pet$Color1, levels = petColors$ColorID, labels = petColors$ColorName)
pet$Color2 <- factor(pet$Color2, levels = petColors$ColorID, labels = petColors$ColorName)
pet$Color3 <- factor(pet$Color3, levels = petColors$ColorID, labels = petColors$ColorName)
## Pet color check
CrossTable(pet$Color1) ## 14993 (100% of records)
CrossTable(pet$Color2) ## 10522
CrossTable(pet$Color3) ## 4389
pet$AdoptionSpeed <- factor(pet$AdoptionSpeed, levels = c(0, 1, 2, 3, 4),
                             labels = c("Same Day", "7 Days", "30 Days", "90 Days", "No Adoption"))
pet$Name <- as.character(pet$Name)
pet$Description <- as.character(pet$Description)
## Reads state labels and assigns
pet$State <- factor(pet$State, levels = stateId$StateID, labels = stateId$StateName)
## State data check
CrossTable(pet$State) ## 14993 (100% of records)
pet$PetID <- as.character(pet$PetID)


## Read and clean test data
petTest <- read.csv('test/test.csv', stringsAsFactors = F)
## Sets factor levels and labels on the appropriate data
petTest$Type <- factor(petTest$Type, levels = c(1, 2), labels = c("Dog", "Cat"))
petTest$Gender <- factor(petTest$Gender, levels =c(1, 2, 3), labels = c("Male", "Female", "Mixed'"))
petTest$Vaccinated <- factor(petTest$Vaccinated, levels = c(1,2,3), labels = c("Yes", "No", "Not Sure"))
petTest$Dewormed <- factor(petTest$Dewormed, levels = c(1,2,3), labels = c("Yes", "No", "Not Sure"))
petTest$Sterilized <- factor(petTest$Sterilized, levels = c(1, 2, 3), labels = c("Yes", "No", "Not Sure"))
petTest$Health <- factor(petTest$Health, levels = c(1, 2, 3, 0), 
                     labels = c("Healthy", "Minor Injury", "Major Injury", "Not Specified"))
petTest$MaturitySize <- factor(petTest$MaturitySize, levels = c(1, 2, 3, 4, 0), 
                           labels = c("Small", "Medium", "Large", "Extra Large", "Not Specified"))
petTest$FurLength <- factor(petTest$FurLength, levels = c(1, 2, 3, 0), 
                        labels = c("Short", "Medium", "Long", "Not Specified"))
## Reads breed labels list and assigns
## May turn 0 into a factor, 0 is currently N/A
petTest$Breed1 <- factor(petTest$Breed1, levels = petBreeds$BreedID, labels = petBreeds$BreedName)
petTest$Breed2 <- factor(petTest$Breed2, levels = petBreeds$BreedID, labels = petBreeds$BreedName)
## Reads color labels list and assigns
## May turn 0 into a factor, 0 is currently N/A
petTest$Color1 <- factor(petTest$Color1, levels = petColors$ColorID, labels = petColors$ColorName)
petTest$Color2 <- factor(petTest$Color2, levels = petColors$ColorID, labels = petColors$ColorName)
petTest$Color3 <- factor(petTest$Color3, levels = petColors$ColorID, labels = petColors$ColorName)
petTest$Name <- as.character(petTest$Name)
petTest$Description <- as.character(petTest$Description)
## Reads state labels and assigns
petTest$State <- factor(petTest$State, levels = stateId$StateID, labels = stateId$StateName)
petTest$PetID <- as.character(petTest$PetID)

## Clean working area by removing read in .csv files
rm(petBreeds)
rm(petColors)
rm(stateId)  


## Structure
str(pet)

## Count of pet types
summary(pet$Type)

## Summary of adoption speeds
summary(pet$AdoptionSpeed)
adoptPct <- table(pet$AdoptionSpeed)
adoptPct <- prop.table(adoptPct) * 100
round(adoptPct, digits = 2)

## Multi-pet groups
table(pet$Quantity)

## min-max normalization function
normalize <- function(x) {
  return ((x-min(x)) / (max(x)-min(x)))
}

## Summarize pet fees
summary(pet$Fee)
table(pet$Fee)
## Remove zero values and normalize pet fees
petNotFree <- pet$Fee[pet$Fee > 0]
summary(petNotFree)
normPetNotFree <- normalize(petNotFree)
summary(normPetNotFree)

## Summarize health
summary(pet$Health)
summary(pet$Vaccinated)
summary(pet$Dewormed)
summary(pet$Sterilized)

## TO DO 1/30
## Group by breed
## Find % of adoption rates by breed (pet$AdoptionSpeed < 5: 1; pet$AdoptionSpeed == 5: 0)
## Watch dplyer tutorial https://www.youtube.com/watch?v=jWjqLW-u3hc
## http://rpubs.com/justmarkham/dplyr-tutorial
## Think about evaluation methods for semester work

## Create boolean adopted variable
pet$AdoptedY <- ifelse(pet$AdoptionSpeed == "No Adoption",0,1)
table(pet$AdoptedY) ## data check
pet$AdoptedY <- factor(pet$AdoptedY, levels = 0:1, labels = c("No", "Yes"))
table(pet$AdoptedY) ## data check

## Group by Breed 1 column
pet %>%
  arrange(Breed1) %>%
  group_by(Breed1)

## TO DO 2/6
## - Verify data clean
## - Do something about missing data
## - Variable clues
##    - Linear model
##    - Bivariate with dependend variable
## - 10 methods to try
## My Tasks:
## Bivariate analyses w/Adoption on
## Age; Color 2(or multicolored); Fur Length; Sterilized; Fee; PhotoAmt

## Age split into wide groups, will look at from multiple angles
plot(pet$Age/12, pet$AdoptionSpeed)
## By age in years
table(round(pet$Age/12), pet$AdoptionSpeed)
CrossTable(round(pet$Age/12), pet$AdoptionSpeed, prop.t = F, prop.chisq = T, chisq = T)
## Pets under 1 year of age are adopted at significantly higher rates within 30 days than those 1 or older


## Cross table of fur length and adoption speed
CrossTable(pet$FurLength, pet$AdoptionSpeed, prop.t = F, prop.chisq = T, chisq = T)
plot(pet$FurLength, pet$AdoptionSpeed)

## Cross table of Color 2 and adoption speed
CrossTable(pet$Color2, pet$AdoptionSpeed, prop.t = F, prop.chisq = T)
## Same day adoption rates between 2.1% - 4.0% for all colors
## Unadopted rates between 25.9% - 31.5% 

## Cross table of sterilization and adoption speed
CrossTable(pet$Sterilized, pet$AdoptionSpeed, prop.t = F, prop.chisq = T)
plot(pet$Sterilized, pet$AdoptionSpeed)
## 67% of pets unsterilized
## 78.4% of unsterilized adopted within 90 days, compared to 58.4% Yes and 58.7% Not Sure
## Same Day adoption rates around the same (2.3% - 3.9%)
## Non-sterilized 7 day rates are double sterilized, 30 days 8.8% higher

## Cross table of photo count and adoption speed
CrossTable(pet$PhotoAmt, pet$AdoptionSpeed, prop.t = F, prop.chisq = F)
plot(pet$PhotoAmt, pet$AdoptionSpeed)
## Adoption speeds seem to dramatically improve with at least one photo and improve steadily with more photos
## Every photo amount over 3 saw a No Adoption rate below the 28% total
## Does not seem to have a significant impact on same day adoption rates
## boolean of pet photos
pet$PhotoY <- ifelse(pet$PhotoAmt == 0, 0, 1)
table(pet$PhotoY) ##data check
pet$PhotoY <- factor(pet$PhotoY, levels = 0:1, labels = c("No", "Yes"))
table(pet$PhotoY) ##data check
## Cross table of if pet has photos and adoption speed
CrossTable(pet$PhotoY, pet$AdoptionSpeed, prop.t = F, prop.chisq = T)
## Adtoption speeds after same day are significantly higher for pets with at least one photo
## 37.5% of pets with no photo adopted within 90 days, 77.8% of pets with at least one photo adopted (28% total unadopted)

## Pet Fee summary and bivariate analysis
summary(pet$Fee)
table(pet$Fee)
table(round(pet$Fee/10))
pet$Free <- ifelse(pet$Fee > 0, 1, 0)
table(pet$Free) ## data check 12663 0s
pet$Free <- factor(pet$Free, 0:1, c("Free", "Not Free"))
table(pet$Free) ## data check
CrossTable(pet$Free, pet$AdoptionSpeed, prop.t = F, prop.chisq = T)
## Nearly equivalent values for free and not free pets in all bands
pet$AvgFee <- ifelse(pet$Fee > 21.26, 1, 0)
pet$AvgFee <- factor(pet$AvgFee, 0:1, c("Below Avg Price", "Above Avg Price"))
CrossTable(pet$AvgFee, pet$AdoptionSpeed, prop.t = F, prop.chisq = T)
petNotFree <- pet$Fee[pet$Fee != 0]
petFree <- pet$Fee[pet$Fee == 0]
summary(petNotFree)

CrossTable(pet$MaturitySize, pet$AdoptionSpeed, prop.chisq = T)
summary(pet$Dewormed)
summary(pet$Type)

## Separate pet quantities by litters (qty > 1) and individuals (qty == 1)
litter = pet$Quantity[pet$Quantity > 1]
singlePet = pet$Quantity[pet$Quantity == 1]
CrossTable(litter, pet$AdoptionSpeed[pet$Quantity > 1])
CrossTable(singlePet, pet$AdoptionSpeed[pet$Quantity == 1])

## Color 1 tests
CrossTable(pet$Color1, pet$AdoptionSpeed)
CrossTable(pet$AdoptionSpeed, pet$Color1)
chisq.test(pet$AdoptionSpeed, pet$Color1)

## Chi-sq test on sex and adoption speed
chisq.test(pet$AdoptionSpeed, pet$Gender) ## x-sq 77.1 p-value 0

## SaveRDS() command
saveRDS(pet, file="pet.csv")
## readRDS reads file
pet <- readRDS("pet.csv")
