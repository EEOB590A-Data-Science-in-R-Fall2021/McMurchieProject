# EEOB590A
# 22 September 2021 
# Data wrangling part 1, practice script ----------

# We will be working with a real insect pan traps dataset that I've amended 
# slightly in order to practice the skills from Monday.  
# The file is called "Data_wrangling_day1_pollination.xlsx"

# 1) Load libraries -----
# you will need tidyverse and readxl
library(tidyverse)
library(readxl)

# 2) Read in data from the InsectData tab --------
#go to file in McMurchieProject > 590A_assignments, then say "import dataset" 
#and copy code preview (only sheet 1)
insect_data <- read_excel("590A_assignments/Data_wrangling_day1_pollination.xlsx", sheet = 1)
View(insect_data)


# 3) Rename columns --------
# Leave columns of insect orders with capital letters, but make all other 
# column names lowercase.
# Need quotations for top color - bowl color due to dashes
colnames(insect_data)
insect_data <- insect_data %>% 
  rename(island = Island, 
         location = Location, 
         tract = Tract,
         "top color - bowl color" = "Top color - Bowl color",
         partial = Partial,
         other = Other)

colnames(insect_data)

# Remove any spaces in column names. Change "location" to "site". 
colnames(insect_data)
insect_data <- insect_data %>% 
  rename(site = location, 
         top_color_bowl_color = "top color - bowl color")

colnames(insect_data)


# Change "tract" to "transect". 
colnames(insect_data)
insect_data <- insect_data %>% 
  rename(transect = tract)

colnames(insect_data)


# 4) Add missing data --------
# Note that the people who entered the data did not drag down the island or 
# location column to fill every row. Double check to make sure this worked correctly.
view(insect_data)
insect_comp <- insect_data %>%
  fill(island, site)

view(insect_comp)


# 5) Separate "Top color - Bowl color" into two different columns ------
# The first letter represents the top color and the second letter represents the 
# bowl color. We do not need to save the original column. 
insect_sep <- insect_comp %>%
  separate(col = top_color_bowl_color, into = c("top_color", "bowl_color"), sep = "-", remove = TRUE) 

view(insect_sep)

# 6) Use the complete function ----------
# Check if we have data for all 3 transects at each location. 
# Do not overwrite the poll data frame when you do this. 
insect_comp2 <- insect_sep %>%
  complete(transect, site)

view(insect_comp2)

# Which transects appear to be missing, and why? 
#L1 LADTG, L2 ladtg, and L3 LADTG are missing, apparently due to a capitalization missmatch

# 7) Unite island, site, transect into a single column -----
# Do not add spaces or punctuation between each part. Call this column uniqueID. 
# Keep the original columns too. 
insect_unite <- insect_sep %>%
  unite(full_location, c(island, site, transect), sep = "_", remove = FALSE)

view(insect_unite)

# 8) Now, make this "wide" dataset into a "long" dataset ---------
# one column should include the insect orders, and one column the number of insects.

insect_long <- insect_unite %>%
  pivot_longer(cols = c(Diptera, Hemiptera, Coleoptera, Formicidae, Apoidea, 
                        Crabronidae, Lepidoptera, Blattodea, Araneae, Isoptera,
                        partial, Trichoptera, other),
               names_to = "orders", values_to = "number_of_insects")

view(insect_long)

# 9) Just to test it out, make your "long" dataset into a "wide" one and see if anything is different. -------

insect_wide <- insect_long %>%
  pivot_wider(names_from = orders, values_from = number_of_insects)

view(insect_wide)

## Are you getting an error? Can you figure out why? 
## I did get an error because values weren't uniquely identified. There are some duplicate values. 
## This results in getting things like "c(2, 0)" for some of the values (list-cols).

# 10) Join the "InsectData" with the "CollectionDates" tab ---------
# Add a collection date for each row of InsectData. You'll need to read in the
# CollectionDates tab. Play around with the various types of 'mutating joins' 
# (i.e. inner_join, left_join, right_join, full_join), to see what each one does
# to the final dataframe, and note which one you think does the job correctly. 

str(insect_long) #checking structure of insect data
insect_dates <- read_excel("590A_assignments/Data_wrangling_day1_pollination.xlsx",
                           sheet = 2) #reading in dates
View(insect_dates)


leftjoin_insects <- insect_long %>%
  left_join(insect_dates, by = c("island", "site")) #left join of insect data and dates
view(leftjoin_insects) #same number of observations as insect_long 6240 obs. of 10 variables
# this one seems right to me - likely don't want fewer observations! 

rightjoin_insects <- insect_long %>%
  right_join(insect_dates, by = c("island", "site")) #right join of insect data and dates
view(rightjoin_insects) #observation number decreased, 5824 obs. of 10 variables 

innerjoin_insects <- insect_long %>%
  inner_join(insect_dates, by = c("island", "site")) #inner join of insect data and dates
view(innerjoin_insects) #equal observations to rightjoin, fewer than leftjoin, 5824 obs. of 10 variables

fulljoin_insects <- insect_long %>%
  full_join(insect_dates, by = c("island", "site")) #left join of insect data and dates
view(fulljoin_insects) #same number of observations as insect_long and left join 6240 obs. of 10 variables


# 11) Create a csv with the long dataframe -------
# dataframe should include collection dates
# put new csv in the data/tidy folder

write.csv(leftjoin_insects, "data/tidy/insect_data_tidy.csv", row.names = FALSE)

insect_datanew <- read_csv("data/tidy/insect_data_tidy.csv") #just checking to make sure this worked
view(insect_datanew) #it did 