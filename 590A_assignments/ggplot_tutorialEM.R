# Introduction to ggplot2 ---------
# Author: Haldre Rogers
# last updated 10 November 2021

# References/Resources --------
# Super useful cheatsheet: https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-visualization.pdf 
# Inspired by several websites, including: https://iqss.github.io/dss-workshops/Rgraphics.html  
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
# 
# Other resources
# http://www.r-graph-gallery.com/ 
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html check out this website for a whole bunch of ideas & code for graphs

# Load Libraries --------
library(tidyverse) #includes ggplot2
library(ggthemes) #may need to install this
library(lme4) #for mixed effects models at the end

# 1. Load Data -------
# Read in seed data and location of seed traps.  
pstraps <- read.csv("~/EEOB590A_DataScienceR/CourseMaterials/data/tidy/pstraps.csv", header = TRUE)
View(pstraps)

# Dataset is to examine the proportion of seeds that are handled relative to distance from the nearest conspecific. This does not include traps that caught 0 seeds, or traps on Guam

# 2. Explore data and identify key variables ---------
str(pstraps)
summary(pstraps)

# Response Variable: 'handled' seeds and 'total' seeds (ignore unhandled)
# create proportion column using these two variables
pstraps$prop <- as.numeric(pstraps$handled/pstraps$total) 
summary(pstraps$prop)

# Predictor variables: 
# island (factor), mindist (distance from nearest conspecific, numeric)

# Other things that might be important
# site (factor)

# 3. Geoms: Using Geometric Objects to display data ---------

## 3.1: Histogram: plot just a single variable -------
# continuous x
ggplot(pstraps, aes(prop)) +
  geom_histogram() #default stat for geom_histogram is "bin"

ggplot(pstraps, aes(prop)) +
  geom_histogram(stat = "bin", bins = 5) #make bin size smaller, looks better here

# categorical x
ggplot(pstraps, aes(island)) +
  geom_bar(stat = "count") #default stat for geom_bar is count. Count takes a
# count of the number of cases, need categorical x variable, and no y-variable.
# can layer aes for individual geom if you have multiple geoms, too, after each geom,
# but then it only applies to one individual geom. 

## 3.2: Boxplot and Violin plot  -------
ggplot(data = pstraps, aes(x = forest.type,y = prop, color = island)) +
  geom_boxplot() #graphs the extreme of the lower whisker, the lower hinge, the median, the upper hinge and the extreme of the upper whisker. The lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles). The upper whisker extends from the hinge to the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers are called "outlying" points and are plotted individually.

ggplot(pstraps, aes(island, prop)) +
  geom_violin() #density boxplot

p1 <- ggplot(data = pstraps, aes(x = island, y = prop)) +
  geom_boxplot() # assign data to p1 (do not graph yet)

p1 + geom_violin(aes(fill = island)) #add a second layer to p1 ggplot to compare violin and boxplots

## 3.3: Barplot ---------
ggplot(data = pstraps, aes(x = island, y = handled) ) +
  geom_bar(stat = "identity") #stat="identity" produces a bar graph of values not counts. Need x and y variables for this. 
#this website is useful http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
#bar graphs usually not best. Imply you started at 0 and counted up
#points with estimate and error bars usually better

## 3.4: Scatterplot -------
ggplot(data = pstraps, aes(x = mindist, y = prop)) +
  geom_point()

ggplot(data = pstraps, aes(x = mindist, y = prop, color = island)) +
  geom_point()

## 3.5: Line graph --------
ggplot(pstraps, aes(mindist, prop)) +
  geom_line() #not very useful. geom_line essentially connects the dots. 

#here's an example that shows where geom_line is useful
ggplot(economics, aes(date, unemploy)) + 
  geom_line()

## 3.6: Vertical and horizontal lines ---------
ggplot(pstraps, aes(mindist, prop)) +
  geom_point() +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 4)

ggplot() +
  geom_hline(yintercept = 0.5) #shortest possible line of code for ggplot graph? 

# 4: Add prediction lines to graph --------
## 4.1: Use model results ------
m1 <- glm(cbind(handled, total-handled) ~ mindist, data = pstraps, family = binomial) 
#fits statistical model to describe relationship between mindist and proportion handled
pstraps$pred <- predict(m1, type = "response") # type = "response" makes sure it's on original scale
# pred for "predict"

ggplot(pstraps, aes(mindist, prop)) +
  geom_point() +
  geom_line(aes(y = pred))

## 4.2: Use stat_smooth ---------
ggplot(pstraps, aes(mindist, prop)) +
  geom_point() +
  geom_smooth() #default method is loess, dark shaded band is +/- confidence intervals.
# Note that these confidence intervals go above 1, which is impossible. 

ggplot(pstraps, aes(mindist, prop)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) 
# much better method for smooth. still gives error bars. 
# Warning bc binomial glm expects integers
# prediction is probably much better though - can actually see statistics

# I'd use predict approach above- this is to show you the geom_smooth option. 

# 5: Add error bars to graph ---------
## 5.1: Add error bars based on error calculated from raw data --------
# First need to calculate error and create new dataframe. Note, this assumes normality. 
sumpstraps <- pstraps %>%
  group_by(island) %>%
  summarise(N = length(total), 
            mean = mean(total), 
            sd   = sd(total), 
            se   = sd / sqrt(N))

ggplot(sumpstraps, aes(island, mean)) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2)

## 5.2: Add error bars based on error from model output -------
m1 <- glm(total ~ island, data = pstraps, family = poisson) #poisson uses a log link

# create dataframe over which to predict model results
island <- c("rota","tinian", "saipan")
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
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2)

#Look here for similar code for binomial model
#http://stats.idre.ucla.edu/r/dae/logit-regression/ 

# 6: Aesthetics: Add/adjust points & lines ---------

## 6.1: add another variable distinguished by color -----
ggplot(pstraps, aes(mindist, prop, color = island)) +
  geom_point()

## 6.2: add another variable distinguished by shape -----
ggplot(pstraps, aes(mindist, prop, shape = island)) +
  geom_point()

ggplot(pstraps, aes(mindist, prop)) +
  geom_point(aes(shape = island)) 
#same as above- this is useful if you have several geoms on a graph, and want different aesthetics for each. 

ggplot(pstraps, aes(mindist, prop, shape = island, color = site)) +
  geom_point() # combination island and site aesthetics

## 6.3: Add two variables, one distinguished by color, another by pointsize -----
ggplot(pstraps, aes(mindist, prop, color = island, size = total)) +
  geom_point() # seeds closer to site are less likely to have been handled (fall right under tree)

## 6.4: create labels instead of points -------
ggplot(pstraps, aes(mindist, prop)) +
  geom_text(aes(label = island), size = 3) #not super useful here, but you can see how this might be helpful

## 6.5: Create graphic with color, point size, and model fit line ----
# first change formula to include island and get predicted values
pstraps$pred <- predict(glm(cbind(handled, total - handled) ~ mindist * island, data = pstraps, family = binomial), type = "response")

ggplot(pstraps, aes(mindist, prop, color = island)) +
  geom_point(aes(size = total)) +
  geom_line(aes(y = pred))

# 7: Scales - Controlling Aesthetic Mapping -------
# Scales adjust the aesthetics we specified above. Can adjust position (e.g. jitter), color, fill, shape, size, linetype here, and then add labels, and change titles, and change breaks in x- and y-axis, as well as limits for x- and y-axes. 

# general function formula is scale_<aesthetic>_<type>, where <aesthetic> is replaced by color, shape, size, y, x etc. and <type> is replaced by continuous, manual, discrete etc. 
# Options include: scale_color_<type>, scale_fill_<type>, scale_size_<type>, scale_shape_<type>, scale_linetype_<type>, scale_x_<type>, scale_y_<type>. 

# this website has a useful table for the options here: http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html#org9900582 
# I'm going to use these colors. 
# Check out http://colorbrewer2.org/ for color scheme ideas
group.colors <- c("saipan" = "#E69F00", "tinian" = "#D55E00FF", "rota" = "#F0E442")

ggplot(pstraps, aes(mindist, prop, color = island)) +
  geom_point() +
  scale_color_manual(name = "Island", breaks = c("rota", "saipan", "tinian"),
                     labels = c("Rota", "Saipan", "Tinian"), values = group.colors)

ggplot(pstraps, aes(mindist, prop, color=island))+
  geom_point()+
  scale_y_continuous(limits=c(0,1), "Proportion seeds without flesh") +
  scale_x_continuous(limits=c(0,20), "Distance to nearest conspecific (m)")

ggplot(pstraps, aes(mindist, prop, color=island))+
  geom_point()+
  scale_color_brewer(palette = "Blues") #use color brewer to choose colors


# 8: Faceting: Create multiple plots from same dataset --------

ggplot(pstraps, aes(mindist, prop))+
  geom_point()+
  facet_wrap(~island)

ggplot(pstraps, aes(mindist, prop))+
  geom_point()+
  facet_grid(.~island)

ggplot(pstraps, aes(mindist, prop))+
  geom_point()+
  facet_grid(island~.)

ggplot(pstraps, aes(mindist, prop))+
  geom_point()+
  facet_grid(island~trap) #not super useful, but you can see how a different factor might be. 

ggplot(pstraps, aes(mindist, prop))+
  geom_point()+
  facet_grid(.~island, scales="free_x") #can let each panel have it's own x or y axes

# 9:Themes - work with axis labels, backgrounds, text size, etc. ------
# This is where you adjust non-data plot elements such as axis labels, plot background, facet label backround, legend appearance

# built-in themes

ggplot(pstraps, aes(mindist, prop, color=island))+
  geom_point(aes(size=total))+
  geom_line(aes(y=pred))+
  theme_classic()

ggplot(pstraps, aes(mindist, prop, color=island))+
  geom_point(aes(size=total))+
  geom_line(aes(y=pred))+
  theme_bw()

ggplot(pstraps, aes(mindist, prop, color=island))+
  geom_point(aes(size=total))+
  geom_line(aes(y=pred))+
  theme_minimal()

# try theme_grey, theme_linedraw, theme_light, theme_fivethirtyeight, theme_economist, theme_few, theme_wsj, theme_tufte

# To change font of all elements of the theme at the same time, use theme_classic(base_size = 12) (can use any specialized theme for this)
ggplot(pstraps, aes(mindist, prop, color=island))+
  geom_point(aes(size=total))+
  geom_line(aes(y=pred))+
  theme_classic(base_size = 10)

# you can adjust aspects of the theme manually
ggplot(pstraps, aes(mindist, prop, color=island))+
  geom_point(aes(size=total))+
  geom_line(aes(y=pred))+
  theme_bw()+
  theme(axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9),
        axis.title.y=element_text(size=9, face="bold"),
        axis.title.x=element_text(size=9),
        axis.line=element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.83,0.5),
        legend.box.just = "left",
        legend.justification = "left",
        legend.key = element_blank(),
        plot.margin = unit(c(1,4,1,1), units="lines"))

#you can also save your own theme.  
#See here: http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html#org01640c8 
# 10: Saving graphs -------
mindist_graph<-ggplot(pstraps, aes(mindist, prop, color=island))+
  geom_point(aes(size=total))+
  geom_line(aes(y=pred))

ggsave("mindist.pdf", width=4, height=4, units="in")

ggsave("mindist.png", width=4, height=4, units="in")

# 11: Put multiple graphs on same figure --------
# egg, cowplot, and gridExtra are packages that are helpful for this
library(egg)
ggarrange(mindist_graph, p1)

library(cowplot) #plot_grid
plot_grid(mindist_graph, p1)

library(gridExtra) #grid.arrange function
grid.arrange(mindist_graph, p1)


######## Other stuff #####################

#Plot model results from mixed effects model
model1ps<-glmer(cbind(handled, total-handled)~island+(1|site), family=binomial, data=pstraps) 

#need inverse logit function
invlogit<-function(x){exp(x)/(1+exp(x))}

###Graph it
#### Confidence intervals are from http://glmm.wikidot.com/faq and from this code: http://glmm.wdfiles.com/local--files/examples/Owls.R

#Get predicted value and upper and lower confidence intervals
#conset up prediction frame
preddata <- with(pstraps, expand.grid(island = levels(island)))

## construct model matrix
mm <- model.matrix(~island,data=preddata)

## predictions from each model; first construct linear
##  predictor, then transform to raw scale
pframe2 <- data.frame(preddata,eta=mm%*%fixef(model1ps))
pframe2 <- with(pframe2,data.frame(pframe2,prop=invlogit(eta)))
pvar1 <- diag(mm %*% tcrossprod(vcov(model1ps),mm))
tvar1 <- pvar1+VarCorr(model1ps)$site  ## must be adapted for more complex models
pframe2 <- data.frame(
  pframe2
  , plo = invlogit(pframe2$eta-2*sqrt(pvar1))
  , phi = invlogit(pframe2$eta+2*sqrt(pvar1))
  , tlo = invlogit(pframe2$eta-2*sqrt(tvar1))
  , thi = invlogit(pframe2$eta+2*sqrt(tvar1))
)

#plot confidence intervals, based on fixed effects uncertainty only (plo and phi)

ggplot(pframe2, aes(x=island, y=prop))+
  geom_point(size=4)+
  geom_rangeframe(data=data.frame(x=c(1,4), y=c(0, 1)), aes(x, y))+
  scale_y_continuous(limits=c(0,1), "Proportion seeds without flesh") +
  scale_x_discrete("", labels=c("Rota", "Saipan", "Tinian"))+
  geom_errorbar(aes(ymin = plo, ymax = phi), width=0.2, size=0.5)+
  annotate("text", label = "a", x = 0.65, y = 1.00, size=3, fontface="bold")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(),
        axis.title.x=element_text(),
        axis.title.y=element_text(size=9, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(1,1,1,1), units="lines"))
