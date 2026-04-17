################################################################################
# Load Packages and Read Data
################################################################################
library(lme4)
library(lmerTest)
library(relaimpo)
library(MuMIn)
library(sjPlot)
library(glmm)
library(tidyverse)
library(car)
library(writexl)


# Read Data --------------------------------------------------------------------
df <- read_csv("Data/no_ysi_or_plankton.csv")



# Modify data to include categorical variables ---------------------------------
# This is using the model dataset without any YSI or Plankton parameters. 
df$vert_pred <- as.character(df$vert_pred)
df$pond <- as.character(df$pond)
df$year <- as.character(df$year)
df$azolla <- as.character(df$azolla)




################################################################################
# Model Selection with Azolla Included (Not in final analysis)
################################################################################
options(na.action = "na.fail")

## Create a global model -------------------------------------------------------
global.model <- lmer(log_larv_dens ~ depth + suitable_598_split + suitable_598 + 
                       vert_pred + log_large_prey + log_invert_pred +log_water_temp + 
                       log_salinity + log_turbidity + log_dist_to_breed + sqrt_chlorophyll + 
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

## Model Averaging with delta AIC ----------------------------------------------
ModelAvg <- model.avg(all_possible_models, subset = delta <= 2)
summary(ModelAvg)
summary(model.avg(ModelAvg, subset = delta <= 2))

## Model averaging with cumulative Akaike weights ------------------------------
ModelAvgAIC <- model.avg(all_possible_models, subset = cumsum(weight) <= 0.95)
summary(ModelAvgAIC)

## Here is the model if we just construct the significant terms from the model averaging
AvgModelAzolla <- lmer(log_larv_dens ~ +(1|pond), data = df)
summary(AvgModelAzolla)
vif(AvgModelAzolla)

## This is the top model from model selection, NOT THE AVERAGE -----------------
TopModel<- lmer(log_larv_dens ~ azolla + depth + log_dist_to_breed + 
                log_invert_pred + log_salinity + vert_pred + (1|pond), 
                data = df)
summary(TopModel)


shapiro.test(residuals(TopModel))
vif(TopModel)
r.squaredGLMM(TopModel)

## Above analysis uses Azolla, which may be important but it was not adequately
## monitored in 2021 and 2022. To rectify this, the code in the next section
## re-runs the above models without Azolla. This model without Azolla is what
## ends up being used for the manuscript; however, I leave this first analysis
## in for transparency.




################################################################################
# Model selection and averaging without Azolla
################################################################################
options(na.action = "na.fail")

## create a global model -------------------------------------------------------
global.model <- lmer(log_larv_dens ~ depth + suitable_598_split + suitable_598 + 
                       vert_pred + log_large_prey + log_invert_pred +
                       log_water_temp + log_salinity + log_turbidity + 
                       log_dist_to_breed + sqrt_chlorophyll + 
                       sqr_emergent_veg + (1|pond), REML = FALSE, data = df)

##Generate all model combinations
all_possible_models_no_azolla <- dredge(global.model)
all_possible_models_no_azolla

##This identifies the top models with delta AIC <= 2
##May be useful for model averaging
subset(all_possible_models_no_azolla, delta <= 2)
## 9 Models
## Depth: 9
## Distance to Nearest Breeding Pond: 9
## Vertebrate Predators: 9
## Salinity: 9
## Emergent Vegetation: 4
## Large Prey: 3
## Invert Predators: 3
## Water Temp: 2


## Model Averaging
ModelAvgNoAzolla <- model.avg(all_possible_models_no_azolla, subset = cumsum(weight) <= 0.95)
summary(ModelAvgNoAzolla)


## Model averaging by delta AIC
ModelAvgDeltAIC <- model.avg(all_possible_models_no_azolla, subset = delta <= 4)
summary(ModelAvgDeltAIC)


##Here is the model if we just construct the top model from the model averaging
TopModelNoAzolla <- lmer(log_larv_dens ~ depth + log_dist_to_breed + log_salinity +
                           vert_pred + (1|pond), data = df)
summary(TopModelNoAzolla)
vif(AvgModelNoAzolla)

## Plot average model
summary(AvgModelNoAzolla)
tidy_model <- augment(AvgModelNoAzolla)


## Spatial Auto Correlation Test -----------------------------------------------
pond_coords <- read_csv("Data/pond_coordinates.csv")


moran_test_data <- data.frame(pond = df$pond, 
                              year = df$year, 
                              residuals = residuals(TopModelNoAzolla)) |>
  group_by(pond) |>
  summarise(residuals = as.numeric(mean(residuals))) |>
  left_join(y = pond_coords, 
            by = join_by(pond),
            unmatched = "drop")

write_xlsx(moran_test_data, "Data/moran_test_data.xlsx")



### This plots the actual datapoints but fits a line to the predicted response
depth_plot <- ggplot(data = tidy_model,
                   mapping = aes(x = depth,
                                 y = .fitted))+
  geom_smooth(method = "lm", level = 0.95, color = "black")+
  geom_point(mapping = aes(x = depth,
                           y = log_larv_dens)) +
  labs(x = "Depth (cm))",
       y = "Larval Density (log larvae/ " ~m^2) +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        title = element_text(size = 20))

depth_plot


distance_plot <- ggplot(data = tidy_model,
                     mapping = aes(x = log_dist_to_breed,
                                   y = .fitted))+
  geom_smooth(method = "lm", level = 0.95, color = "black")+
  geom_point(mapping = aes(x = log_dist_to_breed,
                           y = log_larv_dens)) +
  labs(x = "Distance to Nearest Breeding Pond (log m)",
       y = "Larval Density (log larvae/ " ~m^2) +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        title = element_text(size = 20))

distance_plot


salinity_plot <- ggplot(data = tidy_model,
                        mapping = aes(x = log_salinity,
                                      y = .fitted))+
  geom_smooth(method = "lm", level = 0.95, color = "black")+
  geom_point(mapping = aes(x = log_salinity,
                           y = log_larv_dens)) +
  labs(x = "Salinity (log ppt)",
       y = "Larval Density (log larvae/ " ~m^2) +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        title = element_text(size = 20))

salinity_plot




## Boxplot of azolla model predictions. Still uses fits rather than fixed.
## Note, right now the points being displayed are predictions rather than the actual values
vert_pred_plot <- ggplot(data = tidy_model, aes(x = vert_pred, y = .fitted)) + 
  geom_boxplot() +
  geom_jitter(mapping = aes(x = vert_pred, y = log_larv_dens), width = 0.2) +
  theme_classic()+
  labs(x = "Vertebrate Predator Presence",
       y = "Larval Density (log larvae/ " ~m^2) +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        title = element_text(size = 20))

vert_pred_plot

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

#Does not appear that year needs to be included in these models.################









################################################################################
# Visualizing Akaike weights of variables across all models
################################################################################

## Calculates the sum akaike weights for each variable
sum_weights <- sw(all_possible_models_no_azolla)
sum_weights
weights <- as.data.frame(sum_weights)
weights

weights$variable <- c("Depth", "Distance to Nearest Other Breeding Pond", "Salinity",
                      "Vertebrate Predator Presence", "Emergent Vegetation", "Large Prey Density",
                      "Water Temperature", "Invertebrate Predator Density", 
                      "Suitable and Accessible Habitat", "Suitable Habitat", 
                      "Chlorophyll", "Turbidity")

##Plot cumulative akaike weights for each variables
akaike_weights <- ggplot(weights,
       aes(x = sum_weights,
           y = reorder(variable, sum_weights))) +
  geom_bar(stat = "identity", fill = "black")+
  xlab("Sum Akaike Model Weights")+
  ylab("Predictor")+
  labs(title = "Sum Akaike weights for variables across all possible models")+
  theme_classic(base_size = 32)

akaike_weights

ggsave(plot = akaike_weights,
       filename = "akaike_weights.png",
       path = "Figures/",
       width = 700,
       height = 350,
       units = "mm")
################################################################################





# Data Visualization ###########################################################
sjPlot::plot_model(model = TopModel,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Habitat Variables on SCLTS Larval Density")

sjPlot:: tab_model(TopModel)
