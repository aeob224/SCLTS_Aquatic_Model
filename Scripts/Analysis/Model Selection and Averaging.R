#Load Packages ########################################################################################
library(lme4)
library(lmerTest)
library(relaimpo)
library(MuMIn)
library(sjPlot)
library(glmm)
library(tidyverse)

#Read Data ############################################################################################
df <- read_csv("Data/model_data_no_ysi_or_plankton.csv")



#Modify data to include categorical variables #########################################################
##This is using the model dataset without any YSI or Plankton parameters. 
##Modifying these to be categorical variables
df$vert_pred <- as.character(df$vert_pred)
df$pond <- as.character(df$pond)
df$year <- as.character(df$year)
df$azolla_presence_absence <- as.character(df$azolla_presence_absence)



#Model selection and averaging with all possible models ################################################
options(na.action = "na.fail")

##create a global model
global.model <- lmer(log_larv_dens ~ depth + suitable_598_split + suitable_598 + vert_pred + log_large_prey + log_invert_pred +
                       + log_water_temp + log_salinity + log_turbidity + log_dist_to_breed + sqrt_chlorophyll + 
                       sqrt_emergent_veg + azolla_presence_absence + (1|pond), REML = FALSE, data = df)

##Generate all model combinations
all_possible_models <- dredge(global.model)
all_possible_models

##This identifies the top models with delta AIC <= 2
##May be useful for model averaging
subset(all_possible_models, delta <= 2)
##26 Models
##Azolla: 25
##Depth: 21
##Distance to Nearest Breeding Pond: 21
##Salinity: 19
##Emergent Vegetation: 18
##Vertebrate Predators: 17


##Model Averaging
ModelAvg <- model.avg(all_possible_models, subset = delta <= 2)
summary(ModelAvg)
summary(model.avg(ModelAvg, subset = delta <= 2))

##Model-averaged coefficients:  
##(full average) 
##                            Estimate Std. Error Adjusted SE z value Pr(>|z|)  
##(Intercept)              -7.556e-01  9.508e-01   9.564e-01   0.790   0.4295  
##azolla_presence_absence1 -3.609e-01  1.765e-01   1.782e-01   2.026   0.0428 *
##depth                    -2.776e-03  1.937e-03   1.950e-03   1.424   0.1545  
##log_dist_to_breed        -3.671e-01  2.526e-01   2.543e-01   1.444   0.1488  
##log_invert_pred           4.082e-02  8.208e-02   8.243e-02   0.495   0.6204  
##log_salinity             -3.973e-01  3.118e-01   3.135e-01   1.267   0.2050  
##vert_pred1                2.273e-01  2.088e-01   2.099e-01   1.083   0.2787  
##sqrt_emergent_veg         3.917e-02  3.790e-02   3.810e-02   1.028   0.3040  
##log_large_prey            2.275e-02  7.177e-02   7.213e-02   0.315   0.7524  
##log_water_temp            9.108e-02  4.390e-01   4.411e-01   0.206   0.8364  
##suitable_598_split       -1.425e-08  1.139e-07   1.146e-07   0.124   0.9010  


##Here is the model if we just construct the significant terms from the model averaging
AvgModelAzolla <- lmer(log_larv_dens ~ azolla_presence_absence +(1|pond), data = df)
summary(AvgModelAzolla)
vif(AvgModelAzolla)

##This is the top model from model selection, NOT THE AVERAGE
TopModel<- lmer(log_larv_dens ~ depth + vert_pred + log_invert_pred +
                         + log_salinity + log_dist_to_breed  + 
                         + azolla_presence_absence + (1|pond), data = df)
summary(TopModel)
vif(TopModel)
r.squaredGLMM(TopModel)


##Out of the variables above, only azolla, depth, and distance to nearest breeding pond have p < 0.157.
##Drop all other variables as thy are potentially pretending.
options(na.action = "na.fail")

##create a global model
global.model <- lmer(log_larv_dens ~ depth  + log_dist_to_breed + azolla_presence_absence + 
                       (1|pond), REML = FALSE, data = df)

##Generate all model combinations
all_possible_models_no_pretenders <- dredge(global.model)
all_possible_models_no_pretenders

##This identifies the top models with delta AIC <= 2
##May be useful for model averaging
subset(all_possible_models_no_pretenders, delta <= 2)

##Model Averaging
ModelAvg_no_pretenders <- model.avg(all_possible_models_no_pretenders, subset = delta <= 2)
summary(ModelAvg_no_pretenders)

##Model-averaged coefficients:  
##(full average) 
##                          Estimate Std. Error Adjusted SE z value Pr(>|z|)
##(Intercept)              -0.196175   0.734275    0.738681   0.266    0.791
##azolla_presence_absence1 -0.329200   0.208910    0.210067   1.567    0.117
##log_dist_to_breed        -0.400706   0.258803    0.260504   1.538    0.124
##depth                    -0.001326   0.001796    0.001804   0.735    0.462
 
##################################################################################################################







#Model selection and averaging without Azolla #############################################################
options(na.action = "na.fail")

##create a global model
global.model <- lmer(log_larv_dens ~ depth + suitable_598_split + suitable_598 + vert_pred + log_large_prey + log_invert_pred +
                       + log_water_temp + log_salinity + log_turbidity + log_dist_to_breed + sqrt_chlorophyll + 
                       sqrt_emergent_veg + (1|pond), REML = FALSE, data = df)

##Generate all model combinations
all_possible_models_no_azolla <- dredge(global.model)
all_possible_models_no_azolla

##This identifies the top models with delta AIC <= 2
##May be useful for model averaging
subset(all_possible_models_no_azolla, delta <= 2)
##12 Models
##Depth: 12
##Distance to Nearest Breeding Pond: 12
##Salinity: 10
##Emergent Vegetation: 7
##Vertebrate Predators: 7
##Invert Predators: 2
##Large Prey: 2
##Water Temperature: 2


##Model Averaging
ModelAvgNoAzolla <- model.avg(all_possible_models_no_azolla, subset = delta <= 2)
summary(ModelAvgNoAzolla)
summary(model.avg(ModelAvgNoAzolla, subset = delta <= 2))

##Model-averaged coefficients:  
##(full average) 
##                   Estimate Std. Error Adjusted SE z value Pr(>|z|)   
##(Intercept)       -0.430427   1.047612    1.054594   0.408  0.68317   
##depth             -0.004294   0.001601    0.001618   2.654  0.00795 **
##log_dist_to_breed -0.522064   0.211755    0.214056   2.439  0.01473 * 
##log_salinity      -0.447083   0.292446    0.294451   1.518  0.12892   
##vert_pred1         0.290040   0.204275    0.205566   1.411  0.15826   
##sqrt_emergent_veg  0.026947   0.034771    0.034949   0.771  0.44069   
##log_large_prey     0.028379   0.079559    0.079929   0.355  0.72255   
##log_water_temp     0.203901   0.646664    0.649830   0.314  0.75369   
##log_invert_pred    0.017853   0.055600    0.055888   0.319  0.74938   
 

##Here is the model if we just construct the significant terms from the model averaging
AvgModelNoAzolla <- lmer(log_larv_dens ~ depth + log_dist_to_breed +(1|pond), data = df)
summary(AvgModelNoAzolla)
vif(AvgModelNoAzolla)

##This is the top model from model selection, NOT THE AVERAGE
TopModelNoAzolla <- lmer(log_larv_dens ~ depth + vert_pred +
                         + log_salinity + log_dist_to_breed  + 
                          + (1|pond), data = df)
summary(TopModelNoAzolla)
vif(TopModelNoAzolla)
r.squaredGLMM(TopModelNoAzolla)




##Eliminating pretending variables in the No Azolla process.
##Kept depth, distance to breeding pond, and salinity as they had p values less than 0.157
global.model <- lmer(log_larv_dens ~ depth + log_salinity + log_dist_to_breed +(1|pond), 
                     REML = FALSE, data = df)

##Generate all model combinations
all_possible_models_no_azolla_no_pretending <- dredge(global.model)
all_possible_models_no_azolla_no_pretending

##This identifies the top models with delta AIC <= 2
##May be useful for model averaging
subset(all_possible_models_no_azolla_no_pretending, delta <= 2)
##2 Models
##Depth: 2
##Distance to Nearest Breeding Pond: 2
##Salinity: 1

##Model Averaging
ModelAvgNoAzolla_no_pretending <- model.avg(all_possible_models_no_azolla_no_pretending, subset = delta <= 2)
summary(ModelAvgNoAzolla_no_pretending)

#############################################################################################################

















#Now I am trying the initial work, but I include year as a fixed effect. ############################################################
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

#Testing year as a categorical
yearTopModel <- lmer(log_larv_dens ~ azolla_presence_absence + depth + log_dist_to_breed + 
                       log_salinity + vert_pred + year + (1|pond), data = df)
summary(yearTopModel)
ggplot(data = df,
       mapping = aes(x = year, y = depth))+
  geom_point()+
  stat_smooth(method = "lm", color = "black")
  
Anova(yearLM)

#Does not appear that year needs to be included in these models.#################################################











# Ranking variables by importance on the Azolla selection process ####################################################
# I am concerned that the initial work may be including pretending variables. 
## Variables with lower sum Akaike model weights are more likely to be pretending.

## Calculates the sum akaike weights for each variable
sum_weights <- sw(all_possible_models)

weights$variable <- c("Azolla", "Distance to Breeding Pond", "Depth", "Emergent Vegetation",
                          "Salinity", "Vertebrate Predators", "Invertebrate Predators", "Large Prey",
                          "Water Temp", "Suitable 598m", "Chlorophyll","Suitable 598m Split",
                          "Turbidity")
weights <- as.data.frame(sum_weights)


ggplot(weights,
       aes(x = sum_weights, y = reorder(variable, sum_weights))) +
  geom_bar(stat = "identity")+
  xlab("Sum Akaike Model Weights")+
  ylab("Predictor")
############################################################################






#Ranking variables by importance in the no Azolla model selection process ##################################
sum_weightsNoAzolla <- sw(all_possible_models_no_azolla)
weightsNoAzolla <- as.data.frame(sum_weightsNoAzolla)

weightsNoAzolla$variable <- c("Depth", "Distance to Breeding Pond", "Vertebrate Predators", 
                      "Emergent Vegetation", "Salinity", "Water Temperature",
                      "Large Prey", "Invertebrate Predators", "Chlorophyll",
                      "Suitable 598m", "Suitable 598m Split", "Turbidity")

ggplot(weightsNoAzolla,
       aes(x = sum_weightsNoAzolla, y = variable)) +
  geom_bar(stat = "identity")


ggplot(weightsNoAzolla,
       aes(x = sum_weightsNoAzolla, y = reorder(variable, sum_weightsNoAzolla))) +
  geom_bar(stat = "identity")+
  xlab("Sum Akaike Model Weights")+
  ylab("Predictor")
#########################################################################################

