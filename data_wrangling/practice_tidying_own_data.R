# Load libraries -----
library(tidyverse)
library(readxl)
library(readr)

# Read in data --------
#go to file in McMurchieProject > data > raw, then say "import dataset" 
#and copy code preview (only sheet 1)
foliageleaf <- read_excel("data/raw/BotanyTransformGuaduaSetV3.xlsx")
View(foliageleaf)

# Change column names --------
colnames(foliageleaf)
foliageleaf <- foliageleaf %>% 
  rename(specimen = Specimen,
         country = Country, 
         region = Region, 
         species = Species,
         habit = Habit,
         habitat = Habitat,
         general_habitat = General_habitat)

colnames(foliageleaf)

# Check structure and change columns to factors ------
str(foliageleaf)
foliageleaf <- foliageleaf %>%
  mutate(across(c(specimen, country, region, species, habit, habitat, general_habitat,
                  ab_papillae_long_cells_stomatal, ab_papillae_long_cells_interstomatal,
                  ab_prickle, ab_bimicro,	ab_macro, ab_bilobate_intercostal,
                  ab_ridged_saddle_intercostal, ab_smooth_saddle_intercostal,
                  ab_vertically_elongated_intercostal, ab_bilobate_costal,
                  ab_ridged_saddle_costal, ab_smooth_saddle_costal,	ab_vertically_elongated_costal,
                  ab_triangular_sub_cells, ab_dome_sub_cells,	ab_parallel_sub_cells,
                  ad_stomata,	ad_stomata_freq, ad_papillae_long_cells_stomatal,
                  ad_papillae_long_cells_interstomatal, ad_papillae_overarch,
                  ad_papillae_bulliform, ad_prickle, ad_bimicro, ad_macro,
                  ad_bilobate_intercostal, ad_ridged_saddle_intercostal,
                  ad_smooth_saddle_intercostal, ad_vertically_elongated_intercostal,
                  ad_bilobate_costal, ad_ridged_saddle_costal, ad_smooth_saddle_costal,
                  ad_vertically_elongated_costal, ad_triangular_sub_cells, ad_dome_sub_cells,
                  ad_parallel_sub_cells), as.factor)) # changing multiple columns to factors 
view(foliageleaf)
str(foliageleaf)

## Make all names lowercase ------
foliageleaf <- foliageleaf %>%
  mutate(across(c(specimen, country, region, species, habit, habitat, general_habitat), tolower))
view(foliageleaf)

# Read in lemma and palea data --------
#go to file in McMurchieProject > data > raw, then say "import dataset" 
#and copy code preview (only sheet 1)
flower <- read_excel("data/raw/GuaduaLemmaPalea.xlsx")
View(flower)

# Change column names --------
colnames(flower)
flower <- flower %>% 
  rename(specimen = Specimen,
         country = Country, 
         region = Region, 
         species = Species,
         habit = Habit,
         habitat = Habitat,
         general_habitat = General_habitat)

colnames(flower)

# Check structure and change columns to factors ------
str(flower)
flower <- flower %>%
  mutate(across(c(specimen, species, country, region, habit, habitat, general_habitat,
                  lemma_papillae, lemma_prickle, lemma_bicellular_microhair,
                  lemma_macrohair, lemma_rondel, lemma_cross, lemma_bilobate_vertically_elongated,
                  lemma_bilobate_horizontally_elongated, lemma_vertically_elongated,
                  lemma_horizontally_elongated, lemma_stomata, lemma_stomata_freq,
                  lemma_subsidiary_cells_triangular, lemma_subsidiary_cells_domed,
                  lemma_subsidiary_cells_parallel, palea_papillae, palea_mar_prickle,
                  palea_mar_bicellular_microhair, palea_mar_macrohair, palea_ciliate_keel,
                  palea_sul_prickle, palea_sul_bicellular_microhair, palea_sul_macrohair,
                  palea_rondel, palea_cross, palea_bilobate_vertically_elongated,
                  palea_bilobate_horizontally_elongated, palea_vertically_elongated,
                  palea_horizontally_elongated,	palea_stomata,	palea_stomata_freq,
                  palea_subsidiary_cells_triangular, palea_subsidiary_cells_domed,
                  palea_subsidiary_cells_parallel), as.factor)) # changing multiple columns to factors 
str(flower)

## Make all names lowercase ------
flower <- flower %>%
  mutate(across(c(specimen, country, region, species, habit, habitat, general_habitat), tolower))
view(flower)

## Testing joining, which isn't really practical -----

leftjoin_guadua <- foliageleaf %>%
  left_join(flower, by =  c("specimen", "country", "region", "species", "habit",
                            "habitat", "general_habitat")) #left join of specimens
view(leftjoin_guadua) # gives only specimens where we looked at foliage leaf but includes those where we looked at flowers

rightjoin_guadua <- foliageleaf %>%
  right_join(flower, by =  c("specimen", "country", "region", "species", "habit",
                             "habitat", "general_habitat")) #right join of specimens
view(rightjoin_guadua) # this one is bad - only includes specimens for which we have lemma and palea info,
#which isn't many

innerjoin_guadua <- foliageleaf %>%
  inner_join(flower, by =  c("specimen", "country", "region", "species", "habit",
                             "habitat", "general_habitat")) #inner join of specimens
view(innerjoin_guadua)  # this one is bad - only includes specimens for which we have
#lemma and palea info and foliage leaf info, which is hardly any

fulljoin_guadua <- foliageleaf %>%
  full_join(flower, by = c("specimen", "country", "region", "species", "habit",
                           "habitat", "general_habitat")) #full join of specimens
view(fulljoin_guadua) # gives all specimens we looked at ever

#saving fulljoin version -----
write.csv(fulljoin_guadua, "590A_assignments/fulljoin_guadua.csv", row.names = FALSE)
guadua_full <- read_csv("590A_assignments/fulljoin_guadua.csv") #just checking to make sure this worked
view(guadua_full) #NOTE - have not dealt with NA or x that represents NA just yet. 

#saving foliage leaf version -----
write.csv(foliageleaf, "590A_assignments/foliageleaf.csv", row.names = FALSE)
seeleaf <- read_csv("590A_assignments/foliageleaf.csv") #just checking to make sure this worked
view(seeleaf) #NOTE - have not dealt with NA or x that represents NA just yet. 

#saving flower version -----
write.csv(flower, "590A_assignments/flower.csv", row.names = FALSE)
seeflower <- read_csv("590A_assignments/flower.csv") #just checking to make sure this worked
view(seeflower) #NOTE - have not dealt with NA or x that represents NA just yet. 

