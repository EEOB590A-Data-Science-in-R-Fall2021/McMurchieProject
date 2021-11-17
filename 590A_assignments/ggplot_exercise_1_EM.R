#ggplot Exercise 
#EEOB590

#We will use the forest trajectory dataset to make some graphs. These are from 25m transects conducted across three islands within 4 different forest types. We measured a bunch of things along each transect, so the dataframe is relatively complex. Be sure to use the ggplot_tutorial.R script to help you with this exercise. 

#Load libraries
library(tidyverse)
library(ggthemes)

#Load data
foresttraj <- read.csv("~/EEOB590A_DataScienceR/CourseMaterials/data/tidy/foresttrajectory_site_tidy.csv")

#1) Replicate the figure in the graphics folder called spprich.adult.pdf. 

view(foresttraj)

# Number of adult species is species richness! These are box plots.
scale_color_manual(name = "Island", breaks = c("rota", "saipan", "tinian"),
                   labels = c("Rota", "Saipan", "Tinian"), values = group.colors)

ggplot(data = foresttraj, aes(x = forest.type, y = num.adult.spp, fill = island)) +
  geom_boxplot() +
  scale_fill_brewer(name = "Island", palette = "Blues", labels = c("Guam", "Rota", "Saipan")) +
  scale_y_continuous("Species richness - adult trees") +
  scale_x_discrete("Forest Type", labels = c("Leucana \nthicket", "Mixed introduced \nforest", "Native limestone \nforest", "Scrub-Shrub")) +
  theme_classic()
                   
#2) Now, make a figure based on model output from the model below. The final figure should look like the one called num.adult.trees.pdf.
# Be sure to use the code in the ggplot_tutorial file for this. 

m1 <- glm(num.adults ~ island, data = foresttraj, family = poisson)

# create dataframe over which to predict model results
island <- c("guam","rota", "saipan")
preddata <- as.data.frame(island)

# predict model results
preddata2 <- as.data.frame(predict(m1, newdata = preddata, type = "link", se.fit = TRUE)) 
# type = "link" means printed on log scale
# this means we later need to exponentiate (exp(fit))
preddata2 <- cbind(preddata, preddata2)

# calculate upper and lower CI's
preddata2 <- within(preddata2, {
  pred <- exp(fit)
  lwr <- exp(fit - (1.96 * se.fit))
  upr <- exp(fit + (1.96 * se.fit))
})

ggplot(preddata2, aes(island, pred)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  scale_y_continuous("Number of adult trees per transect") +
  scale_x_discrete("Island", labels = c("Guam", "Rota", "Saipan")) +
  theme_classic() # classic theme has no interior lines or grey bg


#3) Come up with a cool way to visualize the relationship between the number
# of adult species and the number of seedling species across the islands and forest types. 

group.colors <- c("leucaena_thicket" = "#7b3294", "mixed_introduced" = "#c2a5cf",
                  "native_limestone" = "#a6dba0", "scrub_shrub" = "#008837")

# New facet label names for Island
island_labs <- c("Guam", "Rota", "Saipan")
names(island_labs) <- c("guam", "rota", "saipan")

# New facet label names for Forest type
forest_labs <- c("Leucaena thicket", "Mixed introduced", "Native limestone", "Scrub-Shrub")
names(forest_labs) <- c("leucaena_thicket", "mixed_introduced", "native_limestone", "scrub_shrub")


ggplot(foresttraj, aes(mindist, x = num.adult.spp, y = num.seedling.spp, color = forest.type)) +
  geom_point() +
  scale_color_manual(name = "Forest type", breaks = c("leucaena_thicket", "mixed_introduced", "native_limestone", "scrub_shrub"),
                     labels = c("Leucaena thicket", "Mixed introduced", "Native limestone", "Scrub-Shrub"), values = group.colors) +
  scale_y_continuous("Number of seedling species") +
  scale_x_continuous("Number of adult species") +
  facet_grid(island~forest.type, labeller = labeller(island = island_labs, forest.type = forest_labs)) +
  theme_grey()
# here I decided to graph all forest types and islands separately, to get an idea of how patterns differed for each 
# forest and island type (no scrub-shrub on Rota)

#4) Find a cool graphical approach from the websites below, then create a graph
# of that type using data from the foresttraj dataset 
# http://www.r-graph-gallery.com/ 
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html 

# i wanted to make a violin plot of number of adults on each island to see how
# density of number of adults differs by island and forest type

island_labs <- c("Guam", "Rota", "Saipan")
names(island_labs) <- c("guam", "rota", "saipan")

ggplot(data = foresttraj, aes(x = forest.type, y = num.adults, fill = forest.type)) +
  geom_violin() +
  scale_fill_brewer(name = "Forest type", palette = "PuBuGn", labels = c("Leucana thicket", "Mixed introduced forest", "Native limestone forest", "Scrub-Shrub")) +
  scale_y_continuous("Number of adult trees") +
  scale_x_discrete("Forest Type", labels = c("Leucana \nthicket", "Mixed \nintro. \nforest", "Native \nlime-\nstone \nforest", "Scrub-\nShrub")) +
  facet_grid(.~island, labeller = labeller(island = island_labs)) +
  theme_classic()

# I also wanted to make a density plot of number of adults on each island to see
# how density of number of adults differs by Island (ignoring forest type this time)


ggplot(data = foresttraj, aes(x = num.adults, fill = island)) +
  geom_density() +
  scale_fill_brewer(name = "Island", palette = "PuBuGn", labels = c("Guam", "Rota", "Saipan")) +
  scale_y_continuous("Density") +
  scale_x_continuous("Number of adult trees") +
  facet_grid(.~island, labeller = labeller(island = island_labs)) +
  theme_classic()

