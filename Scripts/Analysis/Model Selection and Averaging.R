#Load Packages ########################################################################################
library(lme4)
library(lmerTest)
library(relaimpo)
library(MuMIn)
library(sjPlot)
library(glmm)
library(tidyverse)
library(car)

#Read Data ############################################################################################
df <- read_csv("Data/no_ysi_or_plankton.csv")



#Modify data to include categorical variables #########################################################
##This is using the model dataset without any YSI or Plankton parameters. 
##Modifying these to be categorical variables
df$vert_pred <- as.character(df$vert_pred)
df$pond <- as.character(df$pond)
df$year <- as.character(df$year)
df$azolla <- as.character(df$azolla)



#Model selection and averaging with all possible models ################################################
options(na.action = "na.fail")

##create a global model
global.model <- lmer(log_larv_dens ~ depth + suitable_598_split + suitable_598 + vert_pred + log_large_prey + log_invert_pred +
                       + log_water_temp + log_salinity + log_turbidity + log_dist_to_breed + sqrt_chlorophyll + 
                       sqr_emergent_veg + azolla + (1|pond), REML = FALSE, data = df)

## Generate all model combinations
## This will take a few minutes to run
all_possible_models <- dredge(global.model)
all_possible_models

## This identifies the top models with delta AIC <= 2
## May be useful for model averaging
subset(all_possible_models, delta <= 2)
## 20 Models
## Azolla: 20
## Salinity: 20
## Depth: 18
## Vertebrate Predators: 17
## Distance to breeding pond: 15
## Emergent Vegetation: 13
## Invert predators: 8
## Large Prey: 5
## Water Temp: 3
#3 Suitable hab split: 1

##Model Averaging
ModelAvg <- model.avg(all_possible_models, subset = delta <= 2)
summary(ModelAvg)
summary(model.avg(ModelAvg, subset = delta <= 2))



##Here is the model if we just construct the significant terms from the model averaging
AvgModelAzolla <- lmer(log_larv_dens ~ +(1|pond), data = df)
summary(AvgModelAzolla)
vif(AvgModelAzolla)

##This is the top model from model selection, NOT THE AVERAGE
TopModel<- lmer(log_larv_dens ~ azolla + depth + log_dist_to_breed + 
                log_invert_pred + log_salinity + vert_pred + (1|pond), 
                data = df)
summary(TopModel)

shapiro.test(residuals(TopModel))
vif(TopModel)
r.squaredGLMM(TopModel)
##################################################################################################################




#Model selection and averaging without Azolla ####################################################################
options(na.action = "na.fail")

##create a global model
global.model <- lmer(log_larv_dens ~ depth + suitable_598_split + suitable_598 + vert_pred + log_large_prey + log_invert_pred +
                       + log_water_temp + log_salinity + log_turbidity + log_dist_to_breed + sqrt_chlorophyll + 
                       sqr_emergent_veg + (1|pond), REML = FALSE, data = df)

##Generate all model combinations
all_possible_models_no_azolla <- dredge(global.model)
all_possible_models_no_azolla

##This identifies the top models with delta AIC <= 2
##May be useful for model averaging
subset(all_possible_models_no_azolla, delta <= 2)
##1 Models
## Depth: 11
## Distance to Nearest Breeding Pond: 11
## Vertebrate Predators: 10
## Salinity: 9
## Emergent Vegetation: 6
## Large Prey: 3
## Invert Predators: 3
## Water Temp: 2


##Model Averaging
ModelAvgNoAzolla <- model.avg(all_possible_models_no_azolla, subset = delta <= 2)
summary(ModelAvgNoAzolla)
summary(model.avg(ModelAvgNoAzolla, subset = delta <= 2))


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
sum_weights
weights <- as.data.frame(sum_weights)

weights$variable <- c("Azolla", "Salinity", "Emergent Vegetation", "Depth",
                      "Vertebrate Predators", "Distance to Nearest Breeding Pond", "Invertebrae Predator Density",
                      "Large Prey Density", "Suitable Habitat", "Water Temperature",
                      "Chlorophyll", "Suitable and Accessible Habitat", "Turbidity")

##Plot cumulative akaike weights for each variables
akaike_weights <- ggplot(weights,
       aes(x = sum_weights,
           y = reorder(variable, sum_weights))) +
  geom_bar(stat = "identity", fill = "black")+
  xlab("Sum Akaike Model Weights")+
  ylab("Predictor")+
  labs(title = "Sum Akaike weights for variables in models with \u0394AICc \u2264 2")+
  theme_classic(base_size = 32)

akaike_weights

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
                      "Emergent Vegetation", "Salinity", "Large Prey Density",
                      "Invertebrate Predator Density", "Water Temperature", "Suitable 598m",
                      "Chlorophyll", "Suitable 598m Split", "Turbidity")


ggplot(weightsNoAzolla,
       aes(x = sum_weightsNoAzolla, y = reorder(variable, sum_weightsNoAzolla))) +
  geom_bar(stat = "identity", fill = "black")+
  xlab("Sum Akaike Model Weights")+
  ylab("Predictor")+
  labs(title = "Sum Akaike weights for variables in models with \u0394AICc \u2264 2")+
  theme_classic()
#########################################################################################






# Data Visualization ###########################################################
sjPlot::plot_model(model = TopModel,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Habitat Variables on SCLTS Larval Density")

sjPlot:: tab_model(TopModel)
