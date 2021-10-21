
# Research Question ---------
# Working from a dataset where I observed epidermal features of foliage leaves of
# various Guadua specimens
# Questions: Does habitat, habit, or region affect the presence/absence of different features?

##### Data dictionary ----------
# specimen: Specimen collector and number, with species name included
# country: Country in which the specimen was collected
# region: Region in which specimen was collected: Mexico, Central America, Andes, or Eastern South America
# species: Species name
# habit: Habit typical of species: Small arching, large and erect, or leaning and climbing
# habitat: Habitat typical of species: Forest, river, or savanna
# general_habitat: Habitat typical of species, with river and forest categories combined to include savanna or general
# forest habitats.
# ab_papillae_long_cells_stomatal: Abaxial: papillae on the long cells in the stomatal zone 0 = absent 1 = present
# ab_papillae_long_cells_interstomatal: Abaxial: papillae on the long cells in the interstomatal zone: 0 = absent; 1 =
#  present
# ab_pickle: Abaxial: Prickle hairs: 0 = absent; 1 = present
# ab_bimicro: Abaxial: Bicellular microhairs: 0 = absent; 1 = present
# ab_macro: Abaxial: Macrohairs: 0 = absent; 1 = present
# ab_bilobate_intercostal: Abaxial: Bilobate silica bodies in the intercostal zone: 0 = absent; 1 = present
# ab_ridged_saddle_intercostal: Abaxial: Ridged saddle-shaped silica bodies in the intercostal zone: 0 = absent; 1 =
#  present
# ab_smooth_saddle_intercostal: Abaxial: Smooth saddle-shaped silica bodies in the intercostal zone: 0 = absent; 1 =
#  present
# ab_vertically_elongated_intercostal: Abaxial: Vertically elongated silica bodies in the intercostal zone: 0 = absent; 1
# = present
# ab_bilobate_costal: Abaxial: Bilobate silica bodies in the costal zone: 0 = absent; 1 = present
# ab_ridged_saddle_costal: Abaxial: Ridged saddle-shaped silica bodies in the costal zone: 0 = absent; 1 = present
# ab_smooth_saddle_costal: Abaxial: Smooth saddle-shaped silica bodies in the costal zone: 0 = absent; 1 = present
# ab_vertically_elongated_costal: Abaxial: Vertically elongated silica bodies in the costal zone: 0 = absent; 1 =
#  present
# ab_triangular_sub_cells: Abaxial: Triangular stomata subsidiary cells: 0 = absent; 1 = present
# ab_dome_sub_cells: Abaxial: Dome-shaped stomata subsidiary cells: 0 = absent; 1 = present
# ab_parallel_sub_cells: Abaxial: Parallel-sided stomata subsidiary cells: 0 = absent; 1 = present
# ad_stomata: Adaxial: stomata on the adaxial surface of foliage leaf blades: 0 = absent; 1 = present.
# ad_stomata_freq: Adaxial: Frequency of stomata if present on the adaxial surface of foliage leaf blades: 0 =
#  common; 1 = infrequent.
# ad_papillae_long_cells_stomatal: Adaxial papillae on the long cells in the stomatal zone exclusive of bulliform cells
# 0 = absent 1 = present
# ad_papillae_long_cells_interstomatal: Adaxial papillae on the long cells in the interstomatal zone: 0 = absent; 1 =
#  present.
# ad_papillae_overarch: Adaxial: Papillae on long cells of the intercostal zone adjacent to the stomates: 0 = not
# overarching the stomates; 1 = overarching the stomates.
# ad_papillae_bulliform: Adaxial papillae on the bulliform cells: 0 = absent; 1 = present.
# ad_pickle: Adaxial: Prickle hairs: 0 = absent; 1 = present
# ad_bimicro: Adaxial: Bicellular microhairs: 0 = absent; 1 = present
# ad_macro: Adaxial: Macrohairs: 0 = absent; 1 = present
# ad_bilobate_intercostal: Adaxial: Bilobate silica bodies in the intercostal zone: 0 = absent; 1 = present
# ad_ridged_saddle_intercostal: Adaxial: Ridged saddle-shaped silica bodies in the intercostal zone: 0 = absent; 1 =
#  present
# ad_smooth_saddle_intercostal: Adaxial: Smooth saddle-shaped silica bodies in the intercostal zone: 0 = absent; 1 =
#  present
# ad_vertically_elongated_intercostal: Adaxial: Vertically elongated silica bodies in the intercostal zone: 0 = absent; 1
# = present
# ad_bilobate_costal: Adaxial: Bilobate silica bodies in the costal zone: 0 = absent; 1 = present
# ad_ridged_saddle_costal: Adaxial: Ridged saddle-shaped silica bodies in the costal zone: 0 = absent; 1 = present
# ad_smooth_saddle_costal: Adaxial: Smooth saddle-shaped silica bodies in the costal zone: 0 = absent; 1 = present
# ad_vertically_elongated_costal: Adaxial: Vertically elongated silica bodies in the costal zone: 0 = absent; 1 =
#  present
# ad_triangular_sub_cells: Adaxial: Triangular stomata subsidiary cells: 0 = absent; 1 = present
# ad_dome_sub_cells: Adaxial: Dome-shaped stomata subsidiary cells: 0 = absent; 1 = present
# ad_parallel_sub_cells: Adaxial: Parallel-sided stomata subsidiary cells: 0 = absent; 1 = present

# load library -----
library(tidyverse)
library(skimr)
library(DataExplorer)
library(readr)

# 1. Define your expected dataset ---------
# We need the following columns: 
# response: ab_papillae_long_cells_stomatal,	ab_papillae_long_cells_interstomatal,
# ab_prickle,	ab_bimicro,	ab_macro,	ab_bilobate_intercostal,
# ab_ridged_saddle_intercostal,	ab_smooth_saddle_intercostal,
# ab_vertically_elongated_intercostal,	ab_bilobate_costal,	ab_ridged_saddle_costal,
# ab_smooth_saddle_costal,	ab_vertically_elongated_costal,	ab_triangular_sub_cells,
# ab_dome_sub_cells,	ab_parallel_sub_cells,	ad_stomata,	ad_stomata_freq,	
# ad_papillae_long_cells_stomatal,	ad_papillae_long_cells_interstomatal,
# ad_papillae_overarch,	ad_papillae_bulliform,	ad_prickle,	ad_bimicro,	ad_macro,
# ad_bilobate_intercostal,	ad_ridged_saddle_intercostal,	ad_smooth_saddle_intercostal,
# ad_vertically_elongated_intercostal, ad_bilobate_costal,	ad_ridged_saddle_costal,
# ad_smooth_saddle_costal,	ad_vertically_elongated_costal,	ad_triangular_sub_cells,
# ad_dome_sub_cells,	ad_parallel_sub_cells

# predictors: region, species, habit, habitat

# We sampled regions: Mexico(14), Central America (9), eastern South America (37), Andes (20)
# Species: 22 and 2 unknown
# Habitat: Forest (55), river (11), or savanna (15) (habitat_general combines forest and river)
# Habit: small arching (27), leaning and climbing (14), or big and erect (40)

# Load Dataset ------
# reading x as NA
foliageleaf <- read_csv("590A_assignments/foliageleaf.csv",  na = "x")
View(foliageleaf)


# check structure
str(foliageleaf)

# change some columns to factors
foliageleaf <- foliageleaf %>%
  mutate(across(c(specimen, country, region, species, habit, habitat, general_habitat)
                , as.factor)) # changing multiple columns to factors 

str(foliageleaf)

# Data Exploration with comprehensive functions ---------
# (These might not make as much sense for binary dataset)
skim(foliageleaf) # can see large numbers of missing data for a few response categories
# in those cases it was not applicable

# Trying the create_report() function from DataExplorer package. 
create_report(foliageleaf) # This one doesn't work because there are no continous variables

## Categorical variables (predictors) --------
# Assess sample size. Are there any that have fewer than 15 observations?  

with(foliageleaf, table(region, habit)) # fewer than 15 of each habit in each region

with(foliageleaf, table(region, general_habitat)) # fewer than 15 for savanna and
# Central American forest

with(foliageleaf, table(habit, general_habitat)) # all savanna specimens arching (we knew this)
# also fewer than 15 general forest specimens arching

with(foliageleaf, ftable(region, habit, general_habitat))
# Fewer than 15 specimens for most habits in each region, excluding only big and erect specimens in Andes

# Data Exploration - Relationships between variables ------------

# i chose a few variables of interest to explore here

ggplot(foliageleaf, aes(ab_papillae_long_cells_stomatal, color = general_habitat)) +
  geom_histogram() # FAR more stomatal zone abaxial papillae proportionally in savanna specimens

ggplot(foliageleaf, aes(ab_papillae_long_cells_stomatal, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # a bit complicated but more stomatal zone abaxial papillae
# in leaning and climbing, small arching, and Eastern South American species

ggplot(foliageleaf, aes(ab_papillae_long_cells_interstomatal, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # abaxial interstomatal long cell papillae most common in small arching
# savanna specimens, especially from Eastern South America

ggplot(foliageleaf, aes(ad_papillae_long_cells_stomatal, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # stomatal zone papillae long cells found in all savanna specimens,
# but also some erect and climbing specimens

ggplot(foliageleaf, aes(ad_papillae_long_cells_interstomatal, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # adaxial interstomatal papillae almost exclusively in 
# savanna and small arching specimens, rarely in big and erect spceimens (one?)

ggplot(foliageleaf, aes(ab_ridged_saddle_costal, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # abaxial ridged-saddle shaped silica bodies found mailnly in 
# small arching and savanna species, rarely in leaning and climbing

ggplot(foliageleaf, aes(ad_ridged_saddle_intercostal, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # no abaxial ridged saddle-shaped silica bodies in intercostal at all

ggplot(foliageleaf, aes(ad_ridged_saddle_costal, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # all adaxial ridged saddle-shaped silica bodeis in small
# arching specimens, most in savanna specimens

ggplot(foliageleaf, aes(ab_parallel_sub_cells, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # no obvious pattern for abaxial parallel subsidiary cells

ggplot(foliageleaf, aes(ab_triangular_sub_cells, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # triangular subsidiary cells uncommon - seen more in small arching specimens?

ggplot(foliageleaf, aes(ad_stomata, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # most specimens had some adaxial stomata

ggplot(foliageleaf, aes(ad_stomata_freq, color = general_habitat)) +
  geom_histogram() +
  facet_grid(region ~ habit) # adaxial stomata frequent in most specimens that have them
# more rare only in leaning and climbing specimens

# Summary ----

# Coverage overall is not ideal
# fewer than 15 of each habit in each region

# fewer than 15 for savanna specimens 

# fewer than 15 Central American forest specimens

# all savanna specimens arching (we knew this)

# also fewer than 15 general forest specimens arching
# Fewer than 15 specimens for most habits in each region,
# excluding only big and erect specimens in Andean region

# overall seems like more specimens needed to look into interactions or consider
# regional effects
# prioritize savanna specimens

