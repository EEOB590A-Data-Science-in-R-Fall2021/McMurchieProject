# load appropriate libraries, installing if needed
library(knitr)
library(formatR)
library(RRPP)
library(geomorph)
library(tidyverse)
library(readxl)
library(ade4)
library(vegan)
library(pander)
library(readr)

# Read in data as dataframe
guadualeaf <- read_csv("data/tidy/foliageleafavg.csv", col_names = TRUE, na = "x")
View(guadualeaf)

# drop columns with NA and no variation
guadualeaf <- guadualeaf %>%
  select(-c(ad_stomata_freq, ad_papillae_overarch, ad_triangular_sub_cells,
            ad_dome_sub_cells, ad_parallel_sub_cells, ab_ridged_saddle_intercostal,
            ad_ridged_saddle_intercostal)) 
view(guadualeaf)

df = data.frame(guadualeaf, stringsAsFactors = TRUE)

# Set columns to factors
df <- data.frame(lapply(df,as.factor))

view(df)

## Analysis

### Correlation
Y <- df[,9:ncol(df)] # reads in the binary data only 
Y <- data.frame(lapply(Y,function(x) as.numeric(levels(x))[x]))
correlation_y <- data.frame(cor(Y)) #calculated the correlation of the Y values

# try to write code here to filter by the higher correlation values -----

### PCoA We get the distance matrix for our binary data using simple matching
# coefficient. Then we are able to calculate the PCoA on the distance matrix
# to visualize the pairwise distances between individual Guadua specimen
# micromorphology.

# Distance matrix for binary is simple matching coefficient
Y.dist <- dist.binary(Y, method = 2, diag = FALSE, upper = FALSE)
Y.dist.matrix <- as.matrix(Y.dist)
PCoA <- cmdscale(Y.dist.matrix, eig = TRUE, x.ret = TRUE, list. = TRUE)   #from vegan


# PCoA grouped by habit (3 categories)
nice_df <- PCoA$points %>% 
  data.frame
nice_df$habit <- df$habit
nice_df %>% 
  ggplot(aes(X1, X2)) +
  geom_point(aes(color = habit), size = 2) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    panel.border = element_rect(size = 2, color = "black", fill = NA)
  ) + 
  labs(
    color = "Habit", 
    x = "Axis 1",
    y = "Axis 2"
  )

# PCoA grouped by habit (2 categories)
nice_df <- PCoA$points %>% 
  data.frame
nice_df$general_habit <- df$general_habit
nice_df %>% 
  ggplot(aes(X1, X2)) +
  geom_point(aes(color = general_habit), size = 2) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    panel.border = element_rect(size = 2, color = "black", fill = NA)
  ) + 
  labs(
    color = "Habit", 
    x = "Axis 1",
    y = "Axis 2"
  )

# PCoA grouped by habitat
nice_df <- PCoA$points %>% 
  data.frame
nice_df$habitat <- df$habitat
nice_df %>% 
  ggplot(aes(X1, X2)) +
  geom_point(aes(color = habitat), size = 2) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    panel.border = element_rect(size = 2, color = "black", fill = NA)
  ) + 
  labs(
    color = "Habitat", 
    x = "Axis 1",
    y = "Axis 2"
  )

# PCoA grouped by habitat (2 categories)
nice_df <- PCoA$points %>% 
  data.frame
nice_df$general_habitat <- df$general_habitat
nice_df %>% 
  ggplot(aes(X1, X2)) +
  geom_point(aes(color = general_habitat), size = 2) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    panel.border = element_rect(size = 2, color = "black", fill = NA)
  ) + 
  labs(
    color = "Habitat", 
    x = "Axis 1",
    y = "Axis 2"
  )

# PCoA grouped by region
nice_df <- PCoA$points %>% 
  data.frame
nice_df$region <- df$region
nice_df %>% 
  ggplot(aes(X1, X2)) +
  geom_point(aes(color = region), size = 2) +
  scale_color_viridis_d() + 
  theme_minimal() +
  theme(
    panel.border = element_rect(size = 2, color = "black", fill = NA)
  ) + 
  labs(
    color = "Region", 
    x = "Axis 1",
    y = "Axis 2"
  )

### Scree Plot From the PCoA we are able to obtain eigenvalues and then plot them to show the percent variation explained along each PCoA axis. As seen in the plot below, PCoA axis 1 is responsible for the majority of variation in the PCoA, followed by a steep drop in variation. By the 20th axis, variation is arguably negligible. 

eig_df <- data.frame( x_values = c(1:length(PCoA$eig)) , eig_value = c(PCoA$eig))
ggplot(data = eig_df, aes(x = x_values, y = eig_value)) +
  geom_line() +
  geom_point() + 
  theme_minimal() +
  theme(
    panel.border = element_rect(size = 2, color = "black", fill = NA)
  ) + 
  labs(
    x = "Axis 1",
    y = "Axis 2"
  )

# Factorial MANOVA via RRPP excluding region - 2 groups for each
mydat <- rrpp.data.frame("Y" = Y,
                         "Habit" = as.factor(df$general_habit),
                         "Habitat" = as.factor(df$general_habitat)) 
model1.rrpp <- lm.rrpp(Y.dist.matrix ~ mydat$Habit + mydat$Habitat, 
                       print.progress = FALSE)
anova(model1.rrpp)

# Factorial MANOVA via RRPP excluding region - 3 groups for each 
mydat2 <- rrpp.data.frame("Y" = Y,
                         "Habit" = as.factor(df$habit),
                         "Habitat" = as.factor(df$habitat)) 
model2.rrpp <- lm.rrpp(Y.dist.matrix ~ mydat2$Habit + mydat2$Habitat, 
                       print.progress = FALSE)
anova(model2.rrpp)


#Factorial MANOVA via RRPP with region (two groups for habit and habitat)
mydat3 <- rrpp.data.frame("Y" = Y,
                       "Habit" = as.factor(df$general_habit),
                       "Habitat" = as.factor(df$general_habitat),
                       "Region" = as.factor(df$region)) 
model3.rrpp <- lm.rrpp(Y.dist.matrix ~ mydat3$Habit + mydat3$Habitat + mydat3$Region,  
                       print.progress = FALSE)
anova(model3.rrpp)


### MODEL COMPARISON USING LIKELIHOOD RATIO TEST (LTR)

#### Setup

Y.Habit <- lm.rrpp(Y.dist.matrix ~ mydat3$Habit, 
                 print.progress = FALSE)
Y.Habitat <- lm.rrpp(Y.dist.matrix ~ mydat3$Habitat, 
                   print.progress = FALSE)
#Y.Region <- lm.rrpp(Y.dist.matrix ~ mydat3$Region, 
 #                 print.progress = FALSE)
#Y.Habit.Region <- lm.rrpp(Y.dist.matrix ~ mydat3$Habit + mydat3$Region, 
  #                      print.progress = FALSE)
#Y.Habitat.Region <- lm.rrpp(Y.dist.matrix ~ mydat3$Habitat + mydat3$Region,
   #                       print.progress = FALSE)
Y.Habit.Habitat <- lm.rrpp(Y.dist.matrix ~ mydat3$Habit + mydat3$Habitat,
                         print.progress = FALSE)
#Y.full <- lm.rrpp(Y.dist.matrix ~ mydat3$Habit + mydat3$Habitat + mydat3$Region, 
 #               print.progress = FALSE)

#### RRPP MODEL COMPARISON For our model comparison, we used model.comparison() from the RRPP package using the log likelihood method. From this, we are able to determine that the Y.group2 model is the best fit based on it has the highest log-likelihood score and the lowest AIC score. 
anova(Y.full)

#?RRPP::model.comparison()
modelComp1 <- model.comparison(Y.Habit.Habitat, 
                             Y.Habitat, 
                             Y.Habit, 
                             type = "logLik", tol = 0.01)
modelComp1.summ <- as.data.frame(summary(modelComp1))

pandoc.table(modelComp1.summ,
             style = "grid", 
             plain.ascii = TRUE)

# log likelihood is highest for the model including both habit and habitat but excluding region.