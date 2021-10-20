# Data Exploration and Visualization practice exercise ------
# EEOB590A

# Research Question ---------
# We will be working with a dataset from an experiment where we planted seedlings near and far from conspecific adults and monitored them for survival. 
# Does survival of seedlings depend on distance from nearest conspecific adult, and does that effect vary by species or canopy openness? 

##### Data dictionary ----------
# "species"- six plant species     
# "disp" - disperser present on island - yes/no          
# "island" - island (guam, saipan, tinian, rota)     
# "site"    - 5 sites on Guam, 3 each on Rota, Tinian, Saipan         
# "fence"   - fence name (based on forest plot grid names)       
# "numalive"  - number seedlings alive in fence 
# "date"       - date fence checked     
# "observer"   - person collecting data      
# "dataentry"   - person entering data     
# "dateenter"    - date data entered    
# "uniqueidsppfence" - unique id for each spp:fence combo
# "canopydate"    - date canopy cover data taken 
# "north"          - canopy measurement 1  
# "east"           - canopy measurement 2     
# "south"            - canopy measurement 3  
# "west"             - canopy measurement 4   
# "avgcover"        -average canopy measurement (% cover)    
# "avgopen"          -average canopy measurement (% open)   
# "doubleplant"     - was this fence double planted? 
# "plantdt"          - planting data
# "dist"             - near or far from conspecific? 
# "soil"             - soil type within the fence
# "numseedplant"    - number of seedlings planted
# "DDsurvival_notes"  - notes
# "bird"             - bird presence or absence on the island
# "age"             - age of seedlings (since planting)
# "centavgopen"      - centered average open
# "adultdens_wdisp"  - adult tree density on islands with disperser for that spp
# "adultdens_wodisp" - adult tree density on islands without disperser for that spp
# "seedsize"       - seed size 
# "numtrees"        - number of conspecific trees in the plot 
# "area"            - area of the plot
# "dens_100m"       - calculated density per 100 m
# "regdens"         - density across all plots
# "regdenswd"       - density just from plots with dispersers for that species
####

# Load Libraries -----------
library(tidyverse)
library(skimr)
library(DataExplorer)
library(readr)
library(lubridate)

# Load Dataset ------

# Start with a tidy dataset. Load data/tidy/fencesurv_tidy.csv from the tidy folder. NA values include c("", "NA", "na"). 

fencesurv <- read.csv("~/EEOB590A_DataScienceR/CourseMaterials/data/tidy/fencesurv_tidy.csv")
View(fencesurv)




# 1. Get dataset prepared for exploration ----------

# 1.1: Check structure to make sure everything is in correct class

str(fencesurv)

fencesurv <- fencesurv %>%
  mutate(across(c(species, disp, island, site, fence, observer, dataentry,
                  uniqueidsppfence, doubleplant, dist, soil,
                  bird, disp.y), as.factor)) # changing multiple columns to factors 
str(fencesurv)

?mdy

fencesurv <- fencesurv %>%
  mutate(date = mdy(date)) # mdy = month day year format
str(fencesurv)

fencesurv <- fencesurv %>%
  mutate(dateenter = mdy(dateenter)) # mdy = month day year format

fencesurv <- fencesurv %>%
  mutate(canopydate = dmy(canopydate)) # dmy = day month year format

fencesurv <- fencesurv %>%
  mutate(plantdt = mdy(plantdt)) # mdy = month day year format

str(fencesurv)


# 1.2: If necessary, subset to the dataset you will use for the analysis
# We will use the whole dataset for now, may subset & re-run later. 

# 1.3: Make a new column for proportion alive (propalive) by dividing numalive/numseedplant 

fencesurv <- fencesurv %>%
  mutate(propalive = numalive/numseedplant) %>%
view(fencesurv)
str(fencesurv) # propalive is numeric


# 1.4: Decide which variables are your response variables and which are your predictors
# Response: cbind(numalive, numseedplant) or propalive
# Continuous predictors: centavgopen
# Categorical predictors: species, distance
# Random effects: island (n=4 usually), site (n=3/island)

# 2. Data Exploration with comprehensive functions ---------
# Note anything that stands out to you from these two approaches below. 
# 2.1: Try the skim() functions from the skimr package 


skim(fencesurv)

# 2.2: Try the create_report() function from DataExplorer package. 

create_report(fencesurv)

# 3. Data Exploration: Individual Variables ---------
## 3.1: Continuous variables ---------
# 3.1a: With your continuous response and predictor variables,
# use ggplot and geom_histogram or dotchart() to look for outliers. 

# Response first
ggplot(fencesurv, aes(propalive)) +
  geom_histogram()

ggplot(fencesurv, aes(numalive)) +
  geom_histogram()

ggplot(fencesurv, aes(numseedplant)) +
  geom_histogram() # a whole bunch at 10, some at 5, a few at 9, barely any at 8, otherwise none

# Predictors
ggplot(fencesurv, aes(distance)) +
  geom_histogram()

ggplot(fencesurv, aes(centavgopen)) +
  geom_histogram() # outliers at 40, 60

# 3.1b: With your continuous response variable, look for zero-inflation
# (count data only). Are there more than 25% zero's in the response? 

ggplot(fencesurv, aes(propalive)) +
  geom_histogram() #not more than 25%

# 3.1c: With your continuous response variable, look for independence. 
# Are there patterns in the data that are unrelated to the fixed or random effects identified above?
# Consider patterns over time, for example.

ggplot(fencesurv, aes(x = date, y = propalive)) +
  geom_point() # propalive doesn't seem to vary by date that much

ggplot(fencesurv, aes(site, propalive)) +
  geom_boxplot() +
  facet_grid(.~island)


## 3.2: Categorical variables (predictors) --------
# a) assess whether you have adequate sample size. How many observations per level of each of your categorical predictors? Are there any that have fewer than 15 observations?  

with(fencesurv, table(species, dist)) # all combinations present for species and distance, none fewer than 15 observations.

# Following two just for practice - combinations by site and island
with(fencesurv, ftable(species, dist, site)) # for some sites, not all species present

with(fencesurv, ftable(species, dist, island))# Fewer than 15 of each species present on most islands

# 4. Data Exploration - Relationships between variables ------------

## 4.1: Explore relationships between your predictor variables -------
# 4.1a: look for correlation/covariation between each of your predictors (fixed & random)
#If 2 continuous predictors, use ggplot, geom_point to plot against each other, or use pairs()
#If 1 continuous and 1 categorical predictor, use ggplot with geom_boxplot() 
#For two categorical predictors, use summarize or table (ftable for more than 2 categories)

# Response: cbind(numalive, numseedplant) or propalive
# Continuous predictors: centavgopen
# Categorical predictors: species, distance
# Random effects: island (n=4 usually), site (n=3/island)

ggplot(fencesurv, aes(species, propalive)) + 
  geom_boxplot() # propalive on average very low in papaya and low in premna but there are outliers in papaya

with(fencesurv, table(species, dist))


# 4.1b: Interactions: need to make sure you have adequate data for any 2-way or 3-way interactions in your model. 
## We are interested in a species * distance * centavgopen interaction.
# Do we have adequate sampling across this interaction? 

with(fencesurv, table(species, dist)) #centavpogen is continuous, a different value for every unique ID
# there are at least 26 observations of each species at each distance - this seems adequate

## 4.2: Look at relationships of Y vs X’s ---------
# See if variances are similar for each X value, identify the type of relationship (linear, log, etc.)
#plot each predictor and random effect against the response

# Response: cbind(numalive, numseedplant) or propalive
# Continuous predictors: centavgopen
# Categorical predictors: species, distance
# Random effects: island (n=4 usually), site (n=3/island)

ggplot(fencesurv, aes(species, propalive)) +
  geom_boxplot() # much lower average for papaya, with some outliers at higher propalive
# variances all over the place

ggplot(fencesurv, aes(species, propalive, color = dist)) +
  geom_boxplot() # variances don't differ much between far and near but do differ a LOT by species

ggplot(fencesurv, aes(dist, propalive)) +
  geom_boxplot() #overall, variances fairly similar between near and far

ggplot(fencesurv, aes(species, propalive, color = island))+
  geom_boxplot() +
  facet_grid(.~site) # this is too crowded to see well but shows the large differences in variance

ggplot(fencesurv, aes(centavgopen, propalive)) + # seems to be somewhat of a log relationship here
  geom_point()

ggplot(fencesurv, aes(centavgopen, propalive, color = species)) + # species seems most important predictor of propalive
  geom_point()+
  facet_grid(.~dist)

ggplot(fencesurv, aes(centavgopen, propalive, color = site)) + # site has some effect on propalive but is it based on island?
  geom_point()

ggplot(fencesurv, aes(centavgopen, propalive, color = island)) + # propalive higher on guam and lower on tinian
  geom_point()

ggplot(fencesurv, aes(centavgopen, propalive, color = site)) + # island seems to have more effect than site alone
  geom_point() +
  facet_grid(.~island)

# 5. Summary of data exploration ---------------
# Pull together all of your findings from above to summarize your general results here. This guides you on how to move forward with your analysis. 

## 5.1: Individual variables ---------
### 5.1.a: Continuous variables --------

# 5.1.a.1: Outliers (response & predictors)

# predictor - outliers in centavgopen at around 40 and 60

# for predictor see 
ggplot(fencesurv, aes(centavgopen)) +
geom_histogram() # outliers at 40, 60

# response - don't really see outliers

# for response see
ggplot(fencesurv, aes(propalive)) +
  geom_histogram() # not really seeing outliers in propalive

# 5.1.a.2: Zero-inflation (response)

# although there are ca. 90 zeros, make up less than 25% of the response

# see
ggplot(fencesurv, aes(propalive)) +
  geom_histogram() # zeros are less than 25% of propalive count

# 5.1.a.3: Independence (response)

# island seems to have a big effect on propalive. Island was NOT a predictor, so this isn't good.

# see below - site also related to island
ggplot(fencesurv, aes(centavgopen, propalive, color = island)) + # propalive higher on guam and lower on tinian
  geom_point()

### 5.1.b: Categorical variables ---------
# Typically categorical predictors and random effects
# 5.1.b.1: Sufficient data across all levels (island, soil, species)? Any NA's?

with(fencesurv, table(species, soil))  
# fewer than 15 of neisosperma on karst and soil
# fewer than 15 papaya on karst and soil
# fewer than 15 of psychotria on karst 
# viewing dataset there are some NA for soil

with(fencesurv, table(species, island))
# fewer than 15 neisosperma on Rota and Tinian
# fewer than 15 papaya on guam, Rota (none), and Saipan
# fewer than 15 psychotria on Tinian (none)

with(fencesurv, ftable(species, dist, island)) # fewer than 15 for each species at near or far for all islands but Guam
# fewer than 15 papaya and psychotria at near and far on Guam too

with(fencesurv, ftable(species, island, soil)) # fewer than 15 for most soil types on each island

with(fencesurv, table(species, site)) # fewer than 25 for all species at each site

## 5.2. Multiple  variables -----------
# What is the relationship between variables? 
### 5.2.a: Between predictor variables ----------

# 5.2.a.1: Collinearity: No strong collinearities. Heterogeneity, though. 

# species present varies by island and soil type


# 5.2.a 2: Interactions - do we have enough data? 

# we're lacking data for species and soil type, as well as species and island and everything and site

### 5.2.b: Between each predictor and response ----------
# 5.2.b.1: Linearity & homogeneity- relationship of Y vs X's. 

# the relationship between centavgopen and propalive is not linear
# species seems to affect propalive but distance does not

