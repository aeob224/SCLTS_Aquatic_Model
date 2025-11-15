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

## Generate all model combinations
## This will take a few minutes to run
all_possible_models <- dredge(global.model)
all_possible_models

## This identifies the top models with delta AIC <= 2
## May be useful for model averaging
subset(all_possible_models, delta <= 2)
##24 Models
##Azolla: 23
##Depth: 19
##Distance to Nearest Breeding Pond: 21
##Salinity: 18
##Emergent Vegetation: 17
##Vertebrate Predators: 17


##Model Averaging
ModelAvg <- model.avg(all_possible_models, subset = delta <= 2)
summary(ModelAvg)
summary(model.avg(ModelAvg, subset = delta <= 2))

##Model-averaged coefficients:  
##(full average) 
##                           Estimate Std. Error Adjusted SE z value Pr(>|z|)  
##(Intercept)              -6.725e-01  9.279e-01   9.340e-01   0.720   0.4715  
##azolla_presence_absence1 -3.592e-01  1.765e-01   1.781e-01   2.017   0.0437 *
##depth                    -2.764e-03  1.942e-03   1.955e-03   1.414   0.1574  
##log_dist_to_breed        -3.980e-01  2.419e-01   2.438e-01   1.633   0.1026  
##log_invert_pred           4.741e-02  8.629e-02   8.669e-02   0.547   0.5845  
##log_salinity             -4.118e-01  3.099e-01   3.117e-01   1.321   0.1864  
##vert_pred1                2.663e-01  2.069e-01   2.081e-01   1.280   0.2007  
##sqrt_emergent_veg         3.719e-02  3.720e-02   3.740e-02   0.994   0.3201  
##log_large_prey            2.509e-02  7.489e-02   7.526e-02   0.333   0.7388  
##log_water_temp            8.531e-02  4.259e-01   4.282e-01   0.199   0.8421  
##suitable_598_split       -1.537e-08  1.179e-07   1.187e-07   0.130   0.8970  


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

##Model Averaging delta AICc <=2
ModelAvg_no_pretenders <- model.avg(all_possible_models_no_pretenders, subset = delta <= 2)
summary(ModelAvg_no_pretenders)

##Model-averaged coefficients:  
##(full average) 
##                          Estimate Std. Error Adjusted SE z value Pr(>|z|)
##(Intercept)              -0.196175   0.734275    0.738681   0.266    0.791
##azolla_presence_absence1 -0.329200   0.208910    0.210067   1.567    0.117
##log_dist_to_breed        -0.400706   0.258803    0.260504   1.538    0.124
##depth                    -0.001326   0.001796    0.001804   0.735    0.462
 

## Model average with 95% cumulative weights
ModelAvg_no_pretenders_95 <- model.avg(all_possible_models_no_pretenders, subset = cumsum(weight) <= 0.95)
summary(ModelAvg_no_pretenders_95)

##################################################################################################################




#Model selection and averaging without Azolla ####################################################################
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
##Depth: 10
##Distance to Nearest Breeding Pond: 10
##Salinity: 8
##Emergent Vegetation: 6
##Vertebrate Predators: 9
##Invert Predators: 2
##Large Prey: 2
##Water Temperature:2


##Model Averaging
ModelAvgNoAzolla <- model.avg(all_possible_models_no_azolla, subset = delta <= 2)
summary(ModelAvgNoAzolla)
summary(model.avg(ModelAvgNoAzolla, subset = delta <= 2))

##Model-averaged coefficients:  
##(full average) 
##                   Estimate Std. Error Adjusted SE z value Pr(>|z|)   
##(Intercept)       -0.457023   1.075641    1.082769   0.422  0.67296   
##depth             -0.004281   0.001595    0.001612   2.656  0.00791 **
##log_dist_to_breed -0.531773   0.211097    0.213397   2.492  0.01270 * 
##log_salinity      -0.452086   0.295541    0.297495   1.520  0.12860   
##vert_pred1         0.340381   0.186169    0.187766   1.813  0.06986 . 
##sqrt_emergent_veg  0.027605   0.035020    0.035199   0.784  0.43289   
##log_large_prey     0.032647   0.084411    0.084808   0.385  0.70027   
##log_water_temp     0.225586   0.675299    0.678679   0.332  0.73959   
##log_invert_pred    0.020081   0.058469    0.058776   0.342  0.73261   
 

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
global.model <- lmer(log_larv_dens ~ depth + log_salinity + log_dist_to_breed + vert_pred +
                       (1|pond), REML = FALSE, data = df)

##Generate all model combinations
all_possible_models_no_azolla_no_pretending <- dredge(global.model)
all_possible_models_no_azolla_no_pretending

##This identifies the top models with delta AIC <= 2
##May be useful for model averaging
subset(all_possible_models_no_azolla_no_pretending, delta <= 2)
##1 Model
## All variables included


##Model Averaging using delta AICc
ModelAvgNoAzolla_no_pretending <- model.avg(all_possible_models_no_azolla_no_pretending, subset = delta <= 2)
summary(ModelAvgNoAzolla_no_pretending)


##Model averaging using 95% cumulative weights
ModelAvgNoAzolla_no_pretending_95 <- model.avg(all_possible_models_no_azolla_no_pretending, subset = cumsum(weight) <= 0.95)
summary(ModelAvgNoAzolla_no_pretending_95)


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

weights <- as.data.frame(sum_weights)

weights$variable <- c("Azolla", "Distance to nearest breeding pond", "Depth", "Emergent vegetation",
                          "Salinity", "Vertebrate predators", "Invertebrate predators", "Large prey",
                          "Water temperature", "Suitable habitat within 598m", "Chlorophyll","Suitable habitat within 598m split by roads",
                          "Turbidity")

##Plot cumulative akaike weights for each variables
akaike_weights <- ggplot(weights,
       aes(x = sum_weights,
           y = reorder(variable, sum_weights))) +
  geom_bar(stat = "identity", fill = "black")+
  xlab("Sum Akaike Model Weights")+
  ylab("Predictor")+
  labs(title = "Sum Akaike weights for variables in models with \u0394AICc \u2264 2")+
  theme_classic(base_size = 32)

ggsave(plot = akaike_weights,
       filename = "akaike_weights.png",
       path = "Figures/",
       width = 700,
       height = 350,
       units = "mm")
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
  ylab("Predictor")+
  labs(title = "Sum Akaike weights for variables in models with \u0394AICc \u2264 2")+
  theme_bw()
#########################################################################################






# Data Visualization ###########################################################
sjPlot::plot_model(model = TopModel,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Habitat Variables on SCLTS Larval Density")

sjPlot:: tab_model(TopModel)
