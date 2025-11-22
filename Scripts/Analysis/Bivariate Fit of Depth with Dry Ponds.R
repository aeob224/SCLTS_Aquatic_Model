#Redoing bivariate fits but including ponds that were totally dry. 

#Load Packages
library(lme4)
library(lmerTest)
library(tidyverse)
library(mgcv)
data <- read_csv("Data/data_with_dry_ponds.csv")

##Depth linear mixed effects model 
depth_model <- lmer(log_larv_dens ~ depth + (1|pond), data = data)
summary(depth_model)
#Not significant when three dry ponds are added.



#Depth GAM to account for a potential non-linear response
data$pond <- as.factor(data$pond)
depth_GAM <- gam(log_larv_dens ~ s(depth) + s(pond, bs = 're'), data = data)
summary(depth_GAM)
plot(depth_GAM)
#Significant again, with declines occurring at very shallow or very deep extremes

#Plotting the GAM
plot(ggeffects::ggpredict(depth_GAM, terms = 'depth', facets = FALSE))
