# EEOB590A - Data_wrangling part 3 practice exercise ------

# Part 1: Get set up ----------

## 1.1) Load libraries ----------
library("tidyverse")
library(readr)

## 1.2) Read in data ----------
# From the tidy folder, read in the file you created from last week's assignment on the pollination dataset ("poll_long_tidy.csv")
pollination <- read_csv("data/tidy/poll_long_tidy.csv")
View(poll_long_tidy)

## 1.3) Change the class of each variable if needed ------

str(pollination)

pollination <- pollination %>%
  mutate(number_of_insects = as.numeric(number_of_insects)) %>% # to change class in one column to numeric
  mutate(across(c(full_location, island, site, transect, top_color, bowl_color,
                  orders, order_abrev), as.factor)) # changing multiple columns to factors 
str(pollination)


# Part 2: Subset & summarize --------

## 2.1) Make a new dataframe with just the data from Guam at the racetrack site and name accordingly. --------
(pollination_guamrace <- pollination %>%
   filter(island == "guam", site == "race")) #note the use of  == not =

## 2.2) Make a new dataframe with just the uniqueID, island, site, transect, insectorder, numinsects, and duration columns. --------
(pollination_select <- pollination %>%
   select(full_location, island, site, transect, orders, number_of_insects, duration))
#"full_location" is uniqueID, "orders" is insectorder, and "number_of_insects" is numinsects


## 2.3) With the full database (not the new ones you created in the two previous steps), summarize data, to get: --------

### 2.3.a) a table with the total number of insects at each site, and then arrange rows in descending order --------
pollination %>%
  group_by(site) %>%
  summarize (total = sum(number_of_insects, na.rm = TRUE)) %>% #total insects at each site, removing NA
  arrange(desc(total)) #order by number of insects - this is arranging to view, not changing the permanent order of the factors


### 2.3.b) a table that shows the mean number of insects per observation (bowl) per island, arranged in ascending (smallest first) order --------

pollination %>%
  group_by(island) %>%
  summarize (average = mean(number_of_insects, na.rm = TRUE)) %>% #average insects on each island, removing NA
  arrange((average)) #order by average insects, ascending order

### 2.3.c) a table that shows the min and max number of insects per transect (note that the transects have the same name at each site) --------

pollination %>%
  group_by(transect, site) %>%
  summarize(min_insect = min(number_of_insects, na.rm = TRUE),
            max_insect = max(number_of_insects, na.rm = TRUE))

## 2.4) Figure out which insect order is found across the greatest number of sites and has the most total insects --------

pollination %>%
  group_by(orders) %>% # group by insect orders
  filter(number_of_insects > 0) %>% # remove rows where no insects were found
  summarize(total_insect = max(number_of_insects, na.rm = TRUE), nsite = n_distinct(site))
#Find order with most insects and also count number of sites for each order that have at least one insect

## 2.5) For the insect order with the greatest total number
##of insects and found at the most sites, calculate the mean and sd by site. Include the island name in the final table. --------

pollination %>%
  group_by(site, island) %>%
  filter(orders == "Lepidoptera") %>% # include only Lepidoptera, order with greatest total number of insects found at most sites
  summarize(avg_insect = mean(number_of_insects, na.rm = TRUE),
            sd_insect = sd(number_of_insects, na.rm = TRUE))

## 2.6) Ask a question about the relationship between bowl color and insectorder,
#and then write the code to answer your question. ------

# how does bowl color affect the average number of insects collected and the number of sites from which each order was captured? 
# which bowl color is best for capturing each order? 
pollination %>%
  group_by(bowl_color, orders) %>% # group by bowl color and orders
  filter(number_of_insects > 0) %>% # remove rows where no insects were found
  summarize(avg_insect = mean(number_of_insects, na.rm = TRUE), nsite = n_distinct(site))

# is one bowl color better than the others for capturing the highest number of orders? 
pollination %>%
  group_by(bowl_color) %>% # group by bowl color
  filter(number_of_insects > 0) %>% # remove rows where no insects were found
  summarize(avg_insect = mean(number_of_insects, na.rm = TRUE), norder = n_distinct(orders))
