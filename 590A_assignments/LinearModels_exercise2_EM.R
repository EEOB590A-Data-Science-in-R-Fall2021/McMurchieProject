#Linear Models Exercise 2

library(tidyverse)
library(lme4)
library(ggResidpanel)
library(readr)
library(emmeans)

#We will use the same dataset from last week

traits <- read_csv("~/EEOB590A_DataScienceR/CourseMaterials/data/tidy/tidyREUtraits.csv")
View(traits)

traits <- traits %>%
  filter(!is.na(thickness))

traits <- traits %>%
  filter(species == "aglaia"| species == "aidia" | species == "guamia" | species == "cynometra" | species == "neisosperma" | species == "ochrosia" | species == "premna")  

#1) Let's assess model fit for the model that came out on top for all 4 methods
thick1 <- lm(thickness ~ island*species, data = traits)

#Do data follow the assumptions of:
#1) independence? - Site might affect the thickness to an extent, as could area (area of leaf?)
#2) normality? - Possibly not. The Q-Q plot is very strongly skewed at the ends, and the histogram is not quite normal.
#3) constant variance? - Seems okay - same amount of dots above as below 0 on residual plot
#4) linearity? - Residual plot doesn't show much of a pattern (like a funnel shape), so it's probably linear.

resid_panel(thick1)
resid_xpanel(thick1)

#2) Now let's interpret the results, using each of the methods from last week: 

#Option 1: Traditional hypothesis testing (simplified model). 
#use emmeans to tell whether there are differences between islands for a given species
#which species differ between islands? 
thick1 <- lm(thickness ~ island*species, data = traits) #final model

spisl <- emmeans(thick1, pairwise ~island | species) #to test whether there are differences between islands given species
spisl
summary(spisl)

# Aglaia differs between all islands
# Aidia differs between Guam and Rota and Guam and Saipan but not Rota and Saipan
# Cynometra differs significantly only between Guam and Rota and Rota and Saipan 
# Guamia differs significantly between Guam and Rota and Guam and Saipan but not Rota and Saipan
# Neisosperma differs significantly between all islands
# Ochrosia differs significantly between Guam and Rota and Guam and Saipan but not Roata and Saipan
# Premna differs significantly between all islands


#Option 2: Full model approach. 
#get confidence intervals using emmeans, and determine species

thick1 <- lm(thickness ~ island*species, data = traits) #final model

confint(spisl)

# if CI includes 0, it's not significant 
# CI excludes 0 for Aglaia on all island combinations
# CI excludes 0 for Cynometra for Guam-Rota and Guam-Saipan
# CI excludes 0 for Guamia for Guam-Rota and Guam-Saipan
# CI excludes 0 for Neisosperma for all island combinations
# CI excludes 0 for Ochrosia for Guam-Rota and Guam-Saipan
# CI excludes 0 for Premna for all island combinations

#Option 3: Likelihood Ratio Test approach
#use emmeans to determine whether there are differences between species across all islands
thick1 <- lm(thickness ~ island*species, data = traits) #final model

sp <- emmeans(thick1, pairwise ~ species)
sp #shows p-values for each species combination
# MOSt species combinations are signifiacntly different from each other
# Non-siginificant combinations are: Aidia-Cynometra, Aidia-Guamia, Cynometra- Guamia


#Option 4: Create a full model and all submodels and compare AIC values to choose the best fitting model
#just interpret the best fitting model. 
thick1 <- lm(thickness ~ island*species, data = traits) #final model


thick1 <- lm(thickness ~ island*species, data = traits)
thick2 <- lm(thickness ~ island + species, data = traits)
thick3 <- lm(thickness ~ island, data = traits)
thick4 <- lm(thickness ~ species, data = traits)
thick_null <- lm(thickness ~ 1, data = traits)

AIC(thick1, thick2, thick3, thick4, thick_null) # most negative AIC is thick1 <- this is best fitting model

#interpretation of results: island and species are both important predictor of
# leaf thickness, and there is a significant interaction between island and species.
