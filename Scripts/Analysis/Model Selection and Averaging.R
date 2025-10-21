#Redoing effort to 
library(lme4)
library(lmerTest)
library(relaimpo)
library(MuMIn)
library(sjPlot)
library(glmm)
library(car)
library(tidyverse)
df <- read_csv("model_data_no_ysi_or_plankton.csv")


#This is using the model dataset without any YSI or Plankton parameters. 
#Modifying these to be categorical variables
df$vert_pred <- as.character(df$vert_pred)
df$pond <- as.character(df$pond)
df$year <- as.character(df$year)
df$azolla_presence_absence <- as.character(df$azolla_presence_absence)
#Now let's try all possible models
options(na.action = "na.fail")

#create a global model
global.model <- lmer(log_larv_dens ~ depth + suitable_598_split + suitable_598 + vert_pred + log_large_prey + log_invert_pred +
                       + log_water_temp + log_salinity + log_turbidity + log_dist_to_breed + sqrt_chlorophyll + 
                       sqrt_emergent_veg + azolla_presence_absence + (1|pond), REML = FALSE, data = df)

#Generate all model combinations
all_possible_models <- dredge(global.model)
all_possible_models

#This identifies the top models with delta AIC <= 2
#May be useful for model averaging
subset(all_possible_models, delta <= 2)
#26 Models
#Azolla: 25
#Depth: 21
#Distance to Nearest Breeding Pond: 21
#Salinity: 19
#Emergent Vegetation: 18
#Vertebrate Predators: 17


#Model Averaging
ModelAvg <- model.avg(all_possible_models, subset = delta <= 2)
summary(ModelAvg)
summary(model.avg(ModelAvg, subset = delta <= 2))

#Model-averaged coefficients:  
#(full average) 
#Estimate Std. Error Adjusted SE z value Pr(>|z|)  
#(Intercept)              -7.556e-01  9.508e-01   9.564e-01   0.790   0.4295  
#azolla_presence_absence1 -3.609e-01  1.765e-01   1.782e-01   2.026   0.0428 *
#depth                    -2.776e-03  1.937e-03   1.950e-03   1.424   0.1545  
#log_dist_to_breed        -3.671e-01  2.526e-01   2.543e-01   1.444   0.1488  
#log_invert_pred           4.082e-02  8.208e-02   8.243e-02   0.495   0.6204  
#log_salinity             -3.973e-01  3.118e-01   3.135e-01   1.267   0.2050  
#vert_pred1                2.273e-01  2.088e-01   2.099e-01   1.083   0.2787  
#sqrt_emergent_veg         3.917e-02  3.790e-02   3.810e-02   1.028   0.3040  
#log_large_prey            2.275e-02  7.177e-02   7.213e-02   0.315   0.7524  
#log_water_temp            9.108e-02  4.390e-01   4.411e-01   0.206   0.8364  
#suitable_598_split       -1.425e-08  1.139e-07   1.146e-07   0.124   0.9010  



#Here is the model if we just construct the significant terms from the model averaging
AvgModelAzolla <- lmer(log_larv_dens ~ azolla_presence_absence +(1|pond), data = df)
summary(AvgModelAzolla)
vif(AvgModelAzolla)

#This is the top model from model selection, NOT THE AVERAGE
TopModel<- lmer(log_larv_dens ~ depth + vert_pred + log_invert_pred +
                         + log_salinity + log_dist_to_breed  + 
                         + azolla_presence_absence + (1|pond), data = df)
summary(TopModel)
vif(TopModel)
r.squaredGLMM(TopModel)


















#Now I am trying the above work, but I include year as a fixed effect.
#Previously year had been included as a random effect, but this was producing errors.
#Year may still be important, so we want to test its inclusion in the global model.
options(na.action = "na.fail")
global.model <- lmer(log_larv_dens ~ depth + suitable_598_split + suitable_598 + vert_pred + log_large_prey + log_invert_pred +
                       + log_water_temp + log_salinity + log_turbidity + log_dist_to_breed + sqrt_chlorophyll + 
                       sqrt_emergent_veg + azolla_presence_absence + year + (1|pond), REML = FALSE, data = df)

#Generate all model combinations
all_possible_models_year <- dredge(global.model)
all_possible_models_year

#This identifies the top models with delta AIC <= 2
#May be useful for model averaging
subset(all_possible_models_year, delta <= 2)
#28 Models when year is fixed
#Azolla: 27
#Depth: 27
#Distance to Nearest Breeding Pond: 21
#Salinity: 20
#Year (ordinal): 18
#Vertebrate Predators: 16
#Emergent Vegetation: 14
#Large Prey: 3
#Invert predators: 3
#Turbidity: 1
#Water temp: 1
#Suitable 598: 1

#Model Averaging
ModelAvgYEar <- model.avg(all_possible_models_year, subset = delta <= 2)
summary(ModelAvgYEar)
summary(model.avg(ModelAvg, subset = delta <= 2))

#Testing year as a categoirical
yearTopModel <- lmer(log_larv_dens ~ azolla_presence_absence + depth + log_dist_to_breed + 
                       log_salinity + vert_pred + year + (1|pond), data = df)
summary(yearTopModel)
ggplot(data = df,
       mapping = aes(x = year, y = depth))+
  geom_point()+
  stat_smooth(method = "lm", color = "black")
  
Anova(yearLM)



