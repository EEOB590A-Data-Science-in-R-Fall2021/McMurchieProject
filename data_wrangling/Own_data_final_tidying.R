# load appropriate libraries, installing if needed
library(tidyverse)
library(readr)

# Read in data as dataframe
mydata <- read_csv("data/tidy/foliageleaf.csv", col_names = TRUE, na = "x")
df=data.frame(mydata, stringsAsFactors = TRUE)

# Set columns to factors
df<-data.frame(lapply(df,as.factor))

# Add column for general habit
mydata <- mydata %>%
  mutate(general_habit = habit) %>%
  mutate(general_habit = as.factor(general_habit))
view(mydata)

mydata <- mydata %>%
  mutate(general_habit = fct_recode(general_habit, 
                           "erect_or_climbing" = "big_erect", 
                           "erect_or_climbing" = "leaning_climbing"))
view(mydata)

# Reorder columns
(guadualeaf <- mydata %>%
    select(specimen, country, region, species, habit, general_habit, habitat, general_habitat, everything()))

view(guadualeaf)

# write new csv

write.csv(guadualeaf, "data/tidy/guadualeaf.csv", row.names = FALSE)

guadua <- read_csv("data/tidy/guadualeaf.csv") #just checking to make sure this worked
view(guadua) #it did 