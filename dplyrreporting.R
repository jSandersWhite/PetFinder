test <- readRDS('pet.csv')
library(dplyr)
pet2 <- test %>%
  arrange(Breed1) %>%
  group_by(Breed1) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

pet3 <- test %>%
  filter(as.integer(AdoptionSpeed) %in% c(0,1,2,3)) %>%
  group_by(Breed1) %>%
  summarise(NumAdopted = n()) %>%
  arrange(desc(NumAdopted))

pet4 <- left_join(pet2, pet3, by="Breed1")
pet4$pctAdopt <- (pet4$NumAdopted / pet4$count) * 100
