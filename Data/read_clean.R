#### Read and Clean the Data #######

pet <- read.csv("train.csv", stringsAsFactors = F)
summary(pet)

color <- read.csv("color_labels.csv")
states <- read.csv("state_labels.csv")
breed <- read.csv("breed_labels.csv")

#####   Fix the variable types #####
str(pet) # check data types

pet$Type <- factor(pet$Type, levels = c(1,2), labels = c("dog", "cat"))

pet$Name <- as.character(pet$Name)

pet$Gender <- factor(pet$Gender, levels = c(1,2,3), labels = 
        c("Male", "Female", "Mixed"))
table(pet$Gender) #check record count

######   Check the logic of merging data #######
#test <- pet[1:200,]
#test$Color1 <- factor(test$Color1, levels = color$ColorID, 
#    labels = color$ColorName)
#head(data.frame(test$Color1, pet$Color1[1:200 ]), 50)
#View(color)
#rm(test)  #Seems to be merging correctly

pet$Color1 <- factor(pet$Color1, levels = color$ColorID, 
    labels = color$ColorName)
pet$Color2 <- factor(pet$Color2, levels = color$ColorID, 
    labels = color$ColorName)
pet$Color3 <- factor(pet$Color3, levels = color$ColorID, 
    labels = color$ColorName)

# Take a quick look at colors
#library(gmodels) #Use CrossTables to get record counts
#CrossTable(pet$Color1) #14993 observations
#CrossTable(pet$Color2) #10522 observations
#CrossTable(pet$Color3) #4389 observations
# NOTE: Could create a multi-color variable here

# Logic check for factor pet size
#test <- data.frame(pet$MaturitySize)
#test$word <- factor(pet$MaturitySize, levels = 0:4,
#    labels = c("Not Specified","Small","Medium","Large","Extra Large"))
#summary(test)
#head(test,500)
#test[test$word == "Extra Large", ]
#rm(test) #Merge seems to work, so proceed

## Convert MaturitySize to a factor
pet$MaturitySize <- factor(pet$MaturitySize, levels = 0:4,
    labels = c("Not Specified","Small","Medium","Large","Extra Large")) 
#summary(pet$MaturitySize)

pet$FurLength <- factor(pet$FurLength, levels = 0:3, 
    labels = c("Not Specified", "Short", "Medium", "Long"))
#summary(pet$FurLength)

pet$Vaccinated <- factor(pet$Vaccinated, levels = 1:3,
    labels = c("Yes", "No", "Not Sure"))
#summary(pet$Vaccinated)

#table(pet$Dewormed)
pet$Dewormed <- factor(pet$Dewormed, levels = 1:3,
    labels = c("Yes", "No", "Not Sure"))
#table(pet$Dewormed)

#table(pet$Sterilized)
pet$Sterilized <- factor(pet$Sterilized, levels = 1:3,
    labels = c("Yes", "No", "Not Sure"))
#table(pet$Sterilized)

#table(pet$Health)
pet$Health <- factor(pet$Health, levels = 0:3, 
    labels = c("Not Specified", "Healthy","Minor Injury","Serious Injury"))
#table(pet$Health)

#table(pet$State)
pet$State <- factor(pet$State, levels = states$StateID,
    labels = states$StateName)
#table(pet$State)
#View(states)

pet$AdoptionSpeed <- factor(pet$AdoptionSpeed)

#label breeds

pet$Breed1 <- factor(pet$Breed1, levels = breed$BreedID, 
    labels = breed$BreedName )
pet$Breed1[is.na(pet$Breed1)==T ] <- "unknown"
pet$Breed2 <- factor(pet$Breed2, levels = breed$BreedID, 
    labels = breed$BreedName )

### Save the cleaned file to disk ###
summary(pet)
saveRDS(pet, file = "pet.rds")

#### Do Something with missing values in pet name ##
# Do something with nameless pets
table(pet$Name == "")
table(pet$Name == "No Name")
table(pet$Name == "No Name Yet")
1257+54 #Number of nameless
13736-54+1257 +22

# Test some code for standardizing nameless pets.
check_df <- pet
check_df$Name[check_df$Name == ""] <- "No Name"
table(check_df$Name == "No Name")

library(gmodels)
with(pet, CrossTable(AdoptionSpeed, Type.x , prop.r = F, prop.c = F, 
    prop.chisq = F))
    
