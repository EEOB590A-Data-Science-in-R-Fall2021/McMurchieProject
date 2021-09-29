# EEOB590A - Data_wrangling part 2 practice exercise ------

# Part 1: Get set up ----------

## 1.1) Load libraries ----------
library("tidyverse") #loads dplyr, stringr, ggplot2, tidyr, forcats
library("lubridate")
library(readr)

## 1.2) Read in data ----------
# From the tidy folder, read in the file on pollination you created after finishing last week's assignment
pollination <- read_csv("~/EEOB590A_DataScienceR/McMurchieProject/data/tidy/insect_data_tidy.csv")
View(pollination)

## 1.3) Change name of columns -------
# "date traps out" should be "dateout" and and "date traps coll" should be "datecoll"
pollination <- pollination %>% 
  rename(dateout = "date traps out", 
         datecoll = "date traps coll")
view(pollination)


## 1.4) Change the class of each variable as appropriate ------
# Make variables into factors, numeric, character, etc. Leave the dates as is for now. 
str(pollination)
pollination <- pollination %>%
  mutate(number_of_insects = as.numeric(number_of_insects)) %>% # to change class in one column to numeric
  mutate(across(c(full_location, island, site, transect, top_color, bowl_color,
                  orders), as.factor)) # changing multiple columns to factors 
str(pollination)
view(pollination)


## 1.5) What format are the dates in? Change to date format ----
# Currently they're in month/day/year
# use lubridate to tell R the format of the date
pollination <- pollination %>%
  mutate(dateout = mdy(dateout)) # dmy = day month year format
view(pollination)

pollination <- pollination %>%
  mutate(datecoll = mdy(datecoll)) # dmy = day month year format
view(pollination)

# Part 2: Fix errors within cells ------

## 2.1) Fix the levels of island and site ------
# Make sure all island and site names are in lowercase 
pollination <- pollination %>%
  mutate(across(c(island, site), tolower))
view(pollination)

# Rename sites: forbigrid as forbig and racetrack as race
pollination <- pollination %>%
  mutate(site = fct_recode(site, 
                             "forbig" = "forbigrid", 
                             "race" = "racetrack"))

levels(pollination$site)


## 2.2) Do you see any other errors that should be cleaned up? -----
# I didn't see anything!
# Just good practice to do a final check on this. Insect orders should remain capitalized. 

# Part 3: Create new columns ------

## 3.1: Create a new column for the duration of time traps were out. ------
# Make sure new column is in the numeric class. 
pollination <- pollination %>%
  mutate(duration = datecoll - dateout) %>%
  mutate(duration = as.numeric(duration))
view(pollination)
str(pollination) # duration is numeric


## 3.2: Create a new column with just the first 5 letters of the InsectOrder ------
# Name new column order_abbrev and make sure it is a factor 
pollination <- pollination %>%
  mutate(order_abrev = str_sub(orders, 1,5)) #keep characters in the 1st to 5th position
str(pollination) #it's a character...
pollination <- pollination %>%
  mutate(order_abrev = as.factor(order_abrev)) #making order_abrev a factor
str(pollination)
view(pollination)

# Part 4: Re-arrange levels of a variable and rearrange rows ------
## 4.1) Arrange levels of insectorder by average number of insects. ------ 
#this will let us create a graph later on of insect orders with the most common on one side and least common on the other side of the x-axis.
# Reorder by the frequency of the variable
levels(pollination$orders) #currently alphabetical
pollination <- pollination %>%
  mutate(orders = fct_reorder(orders, number_of_insects)) #order by number of insects
levels(pollination$orders) 


## 4.2) Arrange entire dataset by the number of insects ------
# make these descending, so the greatest number is on row 1. 

pollination <- pollination %>%
  arrange(desc(number_of_insects))
view(pollination)


# Part 5: Print tidied, wrangled database ------
# name file "poll_long_tidy.csv" and put in tidy database
write.csv(pollination, "data/tidy/poll_long_tidy.csv", row.names = FALSE)


