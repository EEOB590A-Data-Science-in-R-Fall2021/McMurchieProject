# Data simulation -------

# We will simulate a dataset for the spider transplant project, as if we are 

# load library -----
library(tidyverse)
library(readr)

# load dataset just for comparison -----
transplant_tidy <- read_csv("~/EEOB590A_DataScienceR/CourseMaterials/data/tidy/transplant_tidy.csv")
view(transplant_tidy)

# 1. Define your expected dataset ---------
# We need the following columns: 
  # response: websize or duration
  # predictors: island, site, netting

# We sampled two islands: Guam & Saipan. 
# Sampled 3 sites on Saipan, 3 sites on Guam
# At each site, we sampled 14 webs
# Half of the webs with netting, half without
# Total of 14*6 = 84 webs

# 2. Simulate response ------------
# we will treat websize as a normal distribution
# we predict that webs will be smaller on Saipan without a net 
gnetwebsize <- rnorm(n = 21, mean = c(54), sd = c(5)) #simulate guam with net
gnonetwebsize <- rnorm(n = 21, mean = c(54), sd = c(5)) #simulate guam without net
snetwebsize <- rnorm(n = 21, mean = c(54), sd = c(5)) #simulate saipan with net
snonetwebsize <- rnorm(n = 21, mean = c(49), sd = c(4)) #simulate saipan without net
websize <- c(gnetwebsize, gnonetwebsize, snetwebsize, snonetwebsize)

# need to set seed if you want the same result every time otherwise it's random
# ex set.seed(30); gnetwebsize <- rnorm(n = 21, mean = c(54), sd = c(5))


# other distributions
gpois <- rpois(n = 21, lambda = 54)
gunif <- runif(n = 21, 40, 64)
gbinom <- rbinom(n = 21, size = 1, .1) # if 80 specimens, use n = 80 not size = 80
# use size = if doing multiple trials

# 2. Simulate predictors ---------

island <- rep(c("guam", "saipan"), each = 42)
gsite <- factor(rep(c("a", "b", "c"), each = 7, times = 2))
ssite <- factor(rep(c("d", "e", "f"), each = 7, times = 2))
site <- c(gsite, ssite) #just need to use c() because both are vectors
netting <- rep(c("yes", "no"), each = 21, times = 2)

# Other helpful code for binomial data
# predictor: 
trtmt <- sample(c("Treated","Untreated"), size = 20, replace = TRUE)
# response
ifelse(trtmt=="Treated", yes = rnorm(20, 10, 1), no = rnorm(20, 20, 1))

# 3. Combine into a dataframe, and save ------
simtransplant <- data.frame(island, site, netting, websize)
write.csv(simtransplant,"data/tidy/simtransplant.csv")

view(simtransplant)

# 4. Graph data ------
ggplot(simtransplant, aes(island, websize, fill=netting))+
  geom_boxplot()

ggplot(simtransplant, aes(websize))+
  geom_histogram(binwidth = 5) +
  facet_grid(netting ~ island)


# 5. Run a model with dataset -------
m1 <- lm(websize ~ netting * island, data = simtransplant)
summary(m1)

# 6. Using tidyverse approach ---------
n_obs = 84 # must use a factor of 4, try 84, 168, 336, 504
tidysimtransplant <- data.frame(uniqueid = seq(1, n_obs, 1)) %>%
  mutate(
    island = rep(c("guam", "saipan"), each = 2, times = n_obs/4),
    #site = rep(c("a", "b", "c", "d", "e", "f"), each = 2, times = n_obs/12),
    netting = rep (c("yes", "no"), each = 1, times = n_obs/2), 
    websize = rnorm(n_obs, mean = c(54, 54, 54, 49), sd = c(5, 5, 5, 4)))

ggplot(tidysimtransplant, aes(island, websize, color = netting)) +
    geom_boxplot()

summary(lm(websize ~ island * netting, data = tidysimtransplant))

#change the mean values and the sample size, and see whether you get a significant result. Run several times at each set of values because each time you run rnorm, you get a new random draw of websizes. 

# binary experiment

#gbinom <- rbinom(n = 21, size = 1, .5)

n_obs = 80 # must use a factor of 4, try 84, 168, 336, 504
bambooex <- data.frame(uniqueid = seq(1, n_obs, 1)) %>%
  mutate(
    location = rep(c("mexico", "central_america", "andes", "sa_east"), each = 2, times = 7, length.out = 80),
    habit = rep(c("erect", "arching", "climbing"), each = 1, times = 8, length.out = 80),
    habitat = rep(c("savanna", "forest"), each = 1, times = n_obs/2), 
    abpapillae = rbinom(n = 80, size = 1, 0.5),
    adpapillae = rbinom(n = 80, size = 1, 0.2),
    ex3 = rbinom(n = 80, size = 1, 0.2),
    ex4 = rbinom(n = 80, size = 1, 0.9),
    ex5 = rbinom(n = 80, size = 1, 0.2),
    ex6 = rbinom(n = 80, size = 1, 0.8),
    ex7 = rbinom(n = 80, size = 1, 0.5),
    ex8 = rbinom(n = 80, size = 1, 0.4),
    ex9 = rbinom(n = 80, size = 1, 0.3),
    ex10 = rbinom(n = 80, size = 1, 0.4),)

view(bambooex)


ggplot(bambooex, aes(abpapillae, color = habitat)) +
  geom_histogram()

ggplot(bambooex, aes(ex10, color = habitat)) +
  geom_histogram() +
  facet_grid(location ~ habit)


