# Coding with Style 
# Fix this script, using good coding convention. 
# This is a made-up dataset, so you can create any story and associated 
# meaningful variable and filenames to go with it. 
# Put this script into the format for the Outline feature in RStudio

# Load packages -----
library(ggplot2)
library(lme4)

# create dataset -----
df1 <- data.frame(state = c('Iowa','Vermont','Hawaii','Texas','Alaska'), 
                  var1 = c('american goldfinch','hermit thrush','nene',
                           'northern mockingbird','willow ptarmigan'), 
                  snow = c(1, 1, 0, 0, 1),
                  number_of_cats = sample(3:10, 5, replace = TRUE))

# explore dataset -----
ggplot(df1, aes(state, number_of_cats)) + geom_point()

## create new column -----
df1$max = 10

# analyze dataset ------
mod1 <- glm(snow ~ number_of_cats, family = binomial, data = df1)
summary(mod1)

# create a csv file ------
write.csv("database1_15September2021.csv")

#### Haldre - you've got great R code style!