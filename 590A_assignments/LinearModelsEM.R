# EEOB590A - Linear models --------
# Spider Web Size Analysis 

# 1. Research Questions and Approach ------- 
# A) Do spiders build smaller webs when birds are present? If so, then web size should be smaller on Saipan than on Guam. Does it depend on whether the web was transplanted or found in place (i.e. native)?
# Response: websize
# Predictors: island, native, island:native
# (optional) Random effect: site
# Approach: Use a linear model without the random effect. I have added a linear mixed effects model (lmer) with the random effect for demonstration purposes. 

# B) Does the spider presence depend on island or netting or the interaction between the two?  

# 2. Load libraries -------
library(ggplot2)
library(emmeans) #for post-hoc test
library(tidyverse)
library(ggResidpanel)
library(car) #note, may need to install "openxlsx" package too and choosing "no" when they ask if you want to install from sources which need compilation. 
library(lmerTest) #automatically loads lme4 package
library(readr)

# 3. Load data -------
transplant <- read.csv("~/EEOB590A_DataScienceR/CourseMaterials/data/tidy/transplant_tidy_clean.csv", header = TRUE)
nrow(transplant) #91 rows
view(transplant)

# 4. Data Exploration ----------------
# Usually, I would like to see the data exploration in the same script as the analysis, but for demonstration purposes, I have separated them. 
# To see the Data Exploration step for this analysis, go to "DataExplorationDay2.R" from last week. 

## Summary of data exploration -------
# no obvious outliers
# can more forward with analysis of web size relative to island and native. 
# there is a lot of variability in web sizes - should consider whether there is something I might or might not have measured related to web size aside from island/native that might affect web size.  

# 5. Fix up dataframe --------
# a.	Remove missing values (NA’s)
# #in this situation, not necessary bc no NA's in predictors or response.

# b. Standardize continuous predictors (if necessary)
# #in this situation, not necessary, bc no continuous predictors


# 6. Analysis ------------

# Do spiders build smaller webs when birds are present? If so, then web size should be smaller on Saipan than on Guam. Does it depend on whether the web was transplanted or found in place (i.e. native)? 

## 6.1: Run linear model --------
# use all data, 
# default for lm is family = gaussian (link = "identity") 

webmod1 <- lm(websize ~ island * native, transplant)
summary(webmod1)
anova(webmod1) #should avoid if unbalanced sample sizes

# note: a linear model is doing the same thing as an anova
webaovmod <- aov(websize ~ island * native, data = transplant)
summary(webaovmod)

# check out the design matrix
model.matrix(webmod1)

## 6.2: Getting to the final model ------
# There are many strategies for model selection, and much disagreement about which approach is best. 

### 6.2.1. Classical Hypothesis testing ------
# drop all nonsignificant predictors, starting with interactions, then main effects. All predictors in final model will be significant. 
webmod1 <- lm(websize ~ island * native, data = transplant)
anova(webmod1) #should avoid if unbalanced sample sizes

#the island * native interaction is not significant, so run model with only main effects. 

webmod1a <- lm(websize ~ island + native, data = transplant) 
anova(webmod1a) #native main effect is not significant

#so final model would be: 
webmod_classic <- lm(websize ~ island, data = transplant)
anova(webmod_classic)

# anova(model) gives Type I sums of squares, which means the reference level is tested first and then other levels, and then interactions. R defaults to treatment contrasts, where first level is baseline, and all others are in reference to that. Can get different results for unbalanced datasets depending on which factor is entered first in the model and thus considered first. For information on Type II and III sums of squares, see the end of this script. 

### 6.2.2. Keep full model --------
# If you do an experiment, don’t do any model selection at all- fit model that you think makes sense, and keep everything as it is, even non-significant parameters. Some might choose to do some model selection to keep only significant interactions, but once fit model with main effect terms, then stick with it. 

webmod1 <- lm(websize ~ island * native, data = transplant)
summary(webmod1)

### 6.2.3: Classic model selection ----------
# Create all sub-models. Use likelihood ratio test to come up with best model. 
webmod1 <- lm(websize ~ island * native, data = transplant)
webmod2 <- lm(websize ~ island + native, data = transplant)
webmod3 <- lm(websize ~ island, data = transplant)
webmod4 <- lm(websize ~ native, data = transplant)
webmod_null <- lm(websize ~ 1, data = transplant)

anova(webmod1, webmod2)  #model 1 not sig better than 2
anova(webmod2, webmod3)  #model 2 not sig better than 3
anova(webmod3, webmod4) #can't run this, because not sub-model - needs to be nested to compare with LRT
anova(webmod3, webmod_null) #model 3 sig better fit than null model
anova(webmod4, webmod_null)

### 6.2.4: Information theoretic approach -------
# compare models using AIC- get competing models. AIC is a measure of the goodness of fit of an estimated statistical model. It is -2*log-likelihood + 2*npar, where npar is the number of effective parameters in the model. More complex models get penalized. 

AIC(webmod1, webmod2, webmod3, webmod4, webmod_null) #webmod3 has lowest AIC, by almost 2 points. Might consider model averaging. 

#check out packages MuMIn and AICcmodavg for a variety of useful tools. 


# 7. Model validation -----------

#super important step -don't overlook! 

# A. Look at homogeneity: plot fitted values vs residuals
# B. Look at influential values: Cook
# C. Look at independence: 
#      plot residuals vs each covariate in the model
#      plot residuals vs each covariate not in the model
#      Common sense 
# D. Look at normality of residuals: histogram

## 7.1 - Use base plot function -------
# for lm can use plot(model), but this doesn't work for glm & glmer

## 7.2 - use ggResidpanel --------
# package was developed by ISU grad students in STATS!
resid_panel(webmod1) # index plot shows pattern over time but observations were ordered by websize so of course 
resid_panel(webmod1, plots = "all")
resid_compare(list(webmod1, webmod3)) #to compare two models
resid_xpanel(webmod1) #to plot residuals against predictor variables

## 7.3 - Pull out fitted and residual values by hand --------
#extract residuals
E1 <- resid(webmod1, type = "pearson")

# plot fitted vs residuals
F1 <- fitted(webmod1, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

# 8. Hypothesis testing -----------
#You have your model. How do you interpret the results? 

## 8.1: anova(model) or summary(model) -------
# If you have only continuous predictors, or two levels within a factor, can use anova(model) or summary(model) 
webmod3<-lm(websize ~ island, data = transplant)
anova(webmod3) #island is a significant predictor. 
summary(webmod3) #spiders on Saipan make webs that are 5.2 cm smaller than those on Guam. 

## 8.2: Post-hoc tests ---------
# If you have multiple levels of a factor, or interactions between factors, will need a post-hoc test to assess differences between levels of the factor or combinations involved in the interaction. There are several options, but emmeans is a good one (also see glht in multcomp). 

#For the purposes of demonstration, we will use the full model with the interaction here, even though the best fitting model has only island as a predictor. 

isl <- emmeans(webmod1, pairwise ~ island*native) # to test whether there are differences between guam & saipan given a particular native status
isl #shows p-value;compare to
summary(webmod1)

isl_contrasts <- isl$contrasts %>%
        summary(infer = TRUE) %>%
        as.data.frame()

isl_contrasts

#can use emmeans to test for main effects when there is an interaction present. 
#emmeans tutorial is here: https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/

natisl <- emmeans(webmod1, pairwise ~native | island) #to test whether there are differences between native & not native given that you are on Guam or Saipan
natisl

## 8.3: Confidence intervals --------
confint(webmod3) #Intercept is guam. The coefficients for Saipan have confidence intervals that do not cross zero, so can conclude Saipan is different from Guam. 

## 8.4: Classic model comparison with Likelihood Ratio Tests --------
#Best fitting model was: 
webmod3<-lm(websize~island, data = transplant)
#can use either Option 1 or Option 2 above. 

## 8.5: Information Theoretic approach ------------
# From 6.2.4 - 
AIC(webmod1, webmod2, webmod3, webmod4, webmod_null) #webmod3 has lowest AIC, by almost 2 points. Might consider model averaging. 

# Best fitting model was: 
webmod3 <- lm(websize ~ island, data = transplant)

#interpretation of results: island is an important predictor of websize, but native is not. Graphically, can see that Saipan webs are smaller than Guam webs.

## 8.6: Sums of Squares and contrasts -------------

##can use car package to do Type II or III sums of squares
Anova(webmod1, type = "III") #Type III tests for the presence of a main effect after the other main effect and interaction. This approach is therefore valid in the presence of significant interactions.However, it is often not interesting to interpret a main effect if interactions are present (generally speaking, if a significant interaction is present, the main effects should not be further analysed).

# explore contrasts
options('contrasts') #shows what contrasts R is using

# can set contrasts to SAS default. "contr.SAS" is a trivial modification of contr.treatment that sets the base level to be the last level instead of the first level. The coefficients produced when using these contrasts should be equivalent to those produced by SAS.

webmod1a <- lm(websize ~ island * native, data = transplant, contrasts = list(island = "contr.SAS", native ="contr.SAS"))
summary(webmod1a)

#Type II
Anova(lm(websize ~ island + native, data = transplant), type = "II")  #This type tests for each main effect after the other main effect.Note that Type II does not handle interactions.

#compare against 
anova(lm(websize ~ native + island, data = transplant))
anova(lm(websize ~ island + native, data = transplant))

# 9. Linear Mixed Effects Model ---------
#in this model, we have added a random effect of site
#we will use lmerTest package. Tutorial is here: http://www2.compute.dtu.dk/courses/02930/SummerschoolMaterialWeb/Readingmaterial/MixedModels-TuesdayandFriday/Packageandtutorialmaterial/lmerTestTutorial.pdf

webmod_mm<-lmer(websize ~ island*native + (1|site), data=transplant)
summary(webmod_mm) 

#Let's remove the non-significant interaction, and then continue with model. 
webmod_mm2<-lmer(websize ~ island+native + (1|site), data=transplant)
summary(webmod_mm2)

#note- could also use the step function to reduce model
step(webmod_mm)

#explore model fit 
resid_panel(webmod_mm2)
resid_xpanel(webmod_mm2)
#model fit is adequate. Normality, linearity, and homogeneity are all ok. 

#Interpret model results
summary(webmod_mm2) #The intercept (Guam, not native) is different from zero (not a big surprise), but Saipan is not different from Guam, and Native="yes" is not different from Native ="no"

confint(webmod_mm2) #to get confidence intervals. 

# 10. Generalized Linear Model -----------

#need to change spidpres to 1's and 0's
transplant$spidpresbin <- ifelse(transplant$spidpres == "yes",1 ,0)

transplant %>%
        count(spidpresbin)

#Run glm with family = binomial 
spidmod <- glm(spidpresbin ~ island*netting, family = binomial, data=transplant)
summary(spidmod) 

#check model fit
resid_panel(spidmod) #QQplot doesn't look very good. I'm not sure why. We will move forward with this for now. 

#interpret results
summary(spidmod) #the effect of netting depends on the island. 

#post-hoc test
islnetting <- emmeans(spidmod, pairwise ~ island*netting)
islnetting #surprisingly, no differences here. 

