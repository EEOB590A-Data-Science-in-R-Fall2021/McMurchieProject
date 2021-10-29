# Linear Model practice exercise

#We are going to work with a dataset on plant traits. We will test whether leaf thickness differs between tree species and island. Today, we will only do data exploration and model building/selection. We will assess model fit and interpret model results next week. 

#Helpful scripts to have open while you work on this include:  DataExplorationDay2.R, and LinearModels.R (from class Tuesday)

#Response: thickness (leaf thickness)
#Predictors: species, island
#Random effects: none for right now, but could add some later

#Load libraries (you'll need the tidyverse)

library(tidyverse)
library(readr)

#Load dataset (tidyREUtraits.csv) and name it "traits".

traits <- read_csv("~/EEOB590A_DataScienceR/CourseMaterials/data/tidy/tidyREUtraits.csv")

### Part 1: explore the dataset  #######

#1. Look at structure of dataset. 

view(traits)

str(traits)

traits <- traits %>%
  mutate(across(c(indiv_ID, island, site, species, indiv), as.factor)) # changing multiple columns to factors 

str(traits)

nrow(traits) # 1927 rows

#2. Subset to rows that have a value for leaf thickness. How many rows did you lose? 

traits <- traits %>%
  drop_na(thickness) #drop any rows with NA's in the specified columns. 


nrow(traits) #1151 rows - now 776 fewer

#Also we will subset to the species that were collected across all three islands. I'll give you the code for this below. 
traits <- traits %>%
  filter(species == "aglaia"| species == "aidia" | species == "guamia" | species == "cynometra" | species == "neisosperma" | species == "ochrosia" | species == "premna")  

## Explore single variables ##
#3. Start with continuous variables - of which we only have the response (thickness)
# a) Check for outliers

ggplot(traits, aes(thickness))+
  geom_histogram() # don't really see any outliers

# b) Check for zero-inflation (is this relevant here?)

# zero inflation is not relevant here as thickness is continuous not count 

# c) Check for independence in the response (is each row independent?).
# Are there some patterns we are not including? 

ggplot(traits, aes(thickness, color=site))+
  geom_boxplot()

# might consider site - there appear to be differences between measurements at different sites

#4. Now categorical predictors. Do you have an adequate sample size?
# How many measurements per level of island and per level of species? 

with(traits, table(island, species)) # some species not found at all. Cynometra found on all islands but only 7 on Guam
# and 8 on Rota.

## Explore relationships between variables
#5) Check for correlations between predictors, or for categorical predictors,
# check to see if the sampling for each species is spread across each island. 
# This is also useful for seeing whether you have adequate samples to run an
# island * species interaction. Try using group_by() and count(), and
# then graphing it using geom_bar() and facet_grid().

#Response: thickness (leaf thickness)
#Predictors: species, island


traitstable <- traits %>%
  group_by(island) %>%
  count(species)

view(traitstable) # Count for Cynometra is <10 for both Guam and Rota, but high on Saipan
# overall Saipan has more of each species

traitstable2 <- traits %>%
  group_by(island) %>%
  count(species)

ggplot(traitstable2, aes(species))+
  geom_bar()+
  facet_grid(.~island) # a bit hard to read - shows collections slightly higher overall on Saipan


#6) Look at relationships of Y vs X’s to see if variances are similar for each X value,
#identify the type of relationship (linear, log, etc.)
#plot each predictor and random effect against the response

#Response: thickness (leaf thickness)
#Predictors: species, island
#Random effects: I'm using area

# checking variance

ggplot(traits, aes(thickness, color = species))+
  geom_boxplot()+
  facet_grid(.~island) # variance seems similar across island, but differs for each species 


#just viewing count of all at each thickness colored by species
ggplot(traits, aes(thickness, color = species))+
  geom_bar()+
  facet_grid(.~island) # can see again saipan has greater thickness overall and greater count for each


# area as random effect
ggplot(traits, aes(area, thickness, color = species))+
  geom_point()+
  facet_grid(.~island) # thickness seems to increase with area almost logarithmically but 
# species has a big effect on thickness



#CONTINUE HERE


### Summary of data exploration ### 
#what did you find? 

### Linear model #### 
# Create a linear model to test whether leaf thickness varies by island, and whether that depends on the plant species. 

#Option 1: Create a full model, remove interaction if not significant, but otherwise do not simplify. 
thickmod1 <- lm(thickness ~ species * island, data = traits)
anova(thickmod1) #interaction is significant 


#Option 2: Create a full model, remove any non-significant interactions to get final model. 

# just saw interaction is significant so this isn't relevant

#Option 3: Create a full model, and all submodels, and compare using Likelihood ratio tests
# (anova(mod1, mod2)) to choose the best fitting model. 

traitmod1 <- lm(thickness ~ species * island, data = traits)
traitmod2 <- lm(thickness ~ species + island, data = traits)
traitmod3 <- lm(thickness ~ species, data = traits)
traitmod4 <- lm(thickness ~ island, data = traits)
traitmod_null <- lm(thickness ~ 1, data = traits)

anova(traitmod1, traitmod2)  #model 1 IS sig better than 2 - model 1 is most complex
# ^ KEEP THIS ONE EVEN THOUGH MOST COMPLEX


# V below for practice
anova(traitmod2, traitmod3)  #model 2 IS sig better than 3
anova(traitmod3, traitmod4) #model 3 sig better than 4
anova(traitmod3, traitmod_null) #model 3 sig better fit than null model
anova(traitmod4, traitmod_null) # model 4 sig better than none

#Option 4: Create a full model and all submodels and compare AIC values to choose the best fitting model

AIC(traitmod1, traitmod2, traitmod3, traitmod4, traitmod_null) 

# most negative AIC is traitmod1 so we keep this model.

#Next week, we will assess model fit, and then interpret results. 
