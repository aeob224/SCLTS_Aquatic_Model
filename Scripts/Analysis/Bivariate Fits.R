#Bivariate Fits of model data

#Load Packages
library(lme4)
library(lmerTest)
library(tidyverse)

#YSI Fits (pH, DO, nitrates)
dat1 <- read_csv("Data/model_data_with_ysi.csv")

##pH model. Not significant
ph_model <- lmer(log_larv_density ~ log_pH + (1|pond), data = dat1)
summary(ph_model)

##DO model. Not significant
DO_model <- lmer(log_larv_density ~ log_DO + (1|pond), data = dat1)
summary(DO_model)


##nitrate_model. Not significant. 
nitrate_model <- lmer(log_larv_density ~ log_nitrates + (1|pond), data = dat1)
summary(nitrate_model)



#Plankton Fits
##Read data
dat2 <- read_csv("Data/model_data_no_ysi.csv")

##Plankton model. Not significant
plankton_model <- lmer(log_larv_dens ~ log_plankton + (1|pond), data = dat2)
summary(plankton_model)


##Medium prey. Not significant
med_prey_model <- lmer(log_larv_dens ~ log_medium_prey + (1|pond), data = dat2)
summary(med_prey_model)



#Everything Else
##Read data
dat3 <- read_csv("model_data_no_ysi_or_plankton.csv")

#Azolla.Significant (p = 0.004)**
azolla_model <- lmer(log_larv_dens ~ azolla_presence_absence + (1|pond), data = dat3)
summary(azolla_model)

#Distance to nearest breeding pond. Significant (p = 0.017) *
distance_model <-  lmer(log_larv_dens ~ log_dist_to_breed + (1|pond), data = dat3)
summary(distance_model)

##Depth. Significant (p = 0.0233)*
depth_model <- lmer(log_larv_dens ~ depth + (1|pond), data = dat3)
summary(depth_model)

#Emergent vegetation. Significant (p = 0.045)*
emergent_veg_model <-  lmer(log_larv_dens ~ sqrt_emergent_veg + (1|pond), data = dat3)
summary(emergent_veg_model)

##Suitable 598m habitat buffer split by roads. Not significant
suitable_split_model <- lmer(log_larv_dens ~ suitable_598_split + (1|pond), data = dat3)
summary(suitable_split_model)

##Suitable habitat 598m. Not significant.
suitable_model <- lmer(log_larv_dens ~ suitable_598 + (1|pond), data = dat3)
summary(suitable_model)

##Vertebrate predators. Not significant.
vert_pred_model <- lmer(log_larv_dens ~ vert_pred + (1|pond), data = dat3)
summary(vert_pred_model)

##Large prey. Not significant.
large_prey_model <- lmer(log_larv_dens ~ log_large_prey + (1|pond), data = dat3)
summary(large_prey_model)

##Invertebrate predators. Not significant.
invert_pred_model <- lmer(log_larv_dens ~ log_invert_pred + (1|pond), data = dat3)
summary(invert_pred_model)

#Water temperature. Not significant
temp_model <- lmer(log_larv_dens ~ log_water_temp + (1|pond), data = dat3)
summary(temp_model)

#Salinity model. Not significant.
salinity_model <- lmer(log_larv_dens ~ log_salinity + (1|pond), data = dat3)
summary(salinity_model)

#Turbidity model. Not significant.
turbidity_model <-  lmer(log_larv_dens ~ log_turbidity + (1|pond), data = dat3)
summary(turbidity_model)

#Chlorophyll. Not significant
chlorophyll_model <- lmer(log_larv_dens ~ sqrt_chlorophyll + (1|pond), data = dat3)
summary(chlorophyll_model)












#Now testing quadratic terms
#YSI Fits (pH, DO, nitrates)

##pH model. Not significant
ph_model_quadratic <- lmer(log_larv_density ~ poly(log_pH,2) + (1|pond), data = dat1)
summary(ph_model_quadratic)

##DO model. Not significant
DO_model_quadratic <- lmer(log_larv_density ~ poly(log_DO,2) + (1|pond), data = dat1)
summary(DO_model_quadratic)


##nitrate_model. Not significant. 
nitrate_model_quadratic <- lmer(log_larv_density ~ poly(log_nitrates,2) + (1|pond), data = dat1)
summary(nitrate_model_quadratic)




#Plankton Fits

##Plankton model. Not significant
plankton_model_quadratic <- lmer(log_larv_dens ~ poly(log_plankton,2) + (1|pond), data = dat2)
summary(plankton_model_quadratic)


##Medium prey. Not significant
med_prey_model_quadratic <- lmer(log_larv_dens ~ poly(log_medium_prey,2) + (1|pond), data = dat2)
summary(med_prey_model_quadratic)



#Everything Else
#Distance to nearest breeding pond. Quadratic term not significant.
distance_model_quadratic <-  lmer(log_larv_dens ~ poly(log_dist_to_breed,2) + (1|pond), data = dat3)
summary(distance_model_quadratic)

##Depth. Quadratic term not significant
depth_model_quadratic <- lmer(log_larv_dens ~ poly(depth,2) + (1|pond), data = dat3)
summary(depth_model_quadratic)

#Emergent vegetation. Not significant
emergent_veg_model_quadratic <-  lmer(log_larv_dens ~ poly(sqrt_emergent_veg,2) + (1|pond), data = dat3)
summary(emergent_veg_model_quadratic)

##Suitable 598m habitat buffer split by roads. Not significant
suitable_split_model_quadratic <- lmer(log_larv_dens ~ poly(suitable_598_split,2) + (1|pond), data = dat3)
summary(suitable_split_model_quadratic)

##Suitable habitat 598m. Not significant.
suitable_model_quadratic <- lmer(log_larv_dens ~ poly(suitable_598,2) + (1|pond), data = dat3)
summary(suitable_model_quadratic)

##Large prey. Not significant.
large_prey_model_quadratic <- lmer(log_larv_dens ~ poly(log_large_prey,2) + (1|pond), data = dat3)
summary(large_prey_model_quadratic)

##Invertebrate predators. Quadratic term significant (p = 0.0116) but linear term is not (p = 0.312)
invert_pred_model <- lmer(log_larv_dens ~ log_invert_pred + (1|pond), data = dat3)
summary(invert_pred_model)

#Water temperature. Quadratic term significant (p = 0.0447), but linear term is not (p = 0.81)
temp_model_quadratic <- lmer(log_larv_dens ~ poly(log_water_temp,2) + (1|pond), data = dat3)
summary(temp_model_quadratic)

#Salinity model. Not significant.
salinity_model_quadratic <- lmer(log_larv_dens ~ poly(log_salinity,2) + (1|pond), data = dat3)
summary(salinity_model_quadratic)

#Turbidity model. Not significant.
turbidity_model_quadratic <-  lmer(log_larv_dens ~ poly(log_turbidity,2) + (1|pond), data = dat3)
summary(turbidity_model_quadratic)

#Chlorophyll. Not significant
chlorophyll_model_quadratic <- lmer(log_larv_dens ~ poly(sqrt_chlorophyll,2) + (1|pond), data = dat3)
summary(chlorophyll_model_quadratic)






