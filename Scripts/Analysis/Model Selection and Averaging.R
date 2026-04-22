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
# Model selection and averaging without Azolla
################################################################################
options(na.action = "na.fail")

## create a global model -------------------------------------------------------
global.model <- lmer(log_larv_dens ~ depth + suitable_598_split + suitable_598 + 
                       vert_pred + log_large_prey + log_invert_pred +
                       log_water_temp + log_salinity + log_turbidity + 
                       log_dist_to_breed + sqrt_chlorophyll + 
                       sqr_emergent_veg + (1|pond), REML = FALSE, data = df)

##Generate all model combinations ----------------------------------------------
all_possible_models_no_azolla <- dredge(global.model)
all_possible_models_no_azolla

model_list_AIC95 <- get.models(all_possible_models_no_azolla, subset = cumsum(weight) <= 0.95)


## Model Averaging -------------------------------------------------------------
ModelAvgNoAzolla <- model.avg(model_list_AIC95)
summary(ModelAvgNoAzolla)

## Calculate and extract residuals for spatial autocorrelation tests -----------
model_average_predictions <- predict(ModelAvgNoAzolla)
model_average_residuals <- df$log_larv_dens - model_average_predictions
df$log_larv_dens[1] - model_average_predictions[1]
## Create residuals for spatial autocorrelation test ---------------------------
pond_coords <- read_csv("Data/pond_coordinates.csv")

# Average Residual at each pond
moran_model_avg_data <- data.frame(pond = df$pond, 
                              year = df$year, 
                              residuals = model_average_residuals) |>
  group_by(pond) |>
  summarise(residuals = as.numeric(mean(residuals))) |>
  left_join(y = pond_coords, 
            by = join_by(pond),
            unmatched = "drop")

write_xlsx(moran_model_avg_data, "Data/Moran Test Data/moran_model_average.xlsx")


# Residual for each survey
moran_model_avg_surveys <- data.frame(pond = df$pond, 
                              year = df$year, 
                              residuals = model_average_residuals) |>
  left_join(y = pond_coords, 
            by = join_by(pond),
            unmatched = "drop")

write_xlsx(moran_model_avg_surveys, "Data/Moran Test Data/moran_model_average_all_points.xlsx")



##This identifies the top models with delta AIC <= 2 ---------------------------
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

## Model averaging by delta AIC
ModelAvgDeltAIC <- model.avg(all_possible_models_no_azolla, subset = delta <= 4)
summary(ModelAvgDeltAIC)


##Here is the model if we just construct the top model from the model averaging
TopModelNoAzolla <- lmer(log_larv_dens ~ depth + log_dist_to_breed + log_salinity +
                           vert_pred + (1|pond), data = df)
summary(TopModelNoAzolla)
vif(TopModelNoAzolla)


## Spatial Auto Correlation Test for Top Model----------------------------------
pond_coords <- read_csv("Data/pond_coordinates.csv")

# Average residuals at each pond
moran_top_model <- data.frame(pond = df$pond, 
                              year = df$year, 
                              residuals = residuals(TopModelNoAzolla)) |>
  group_by(pond) |>
  summarise(residuals = as.numeric(mean(residuals))) |>
  left_join(y = pond_coords, 
            by = join_by(pond),
            unmatched = "drop")

write_xlsx(moran_top_model, "Data/Moran Test Data/moran_top_model.xlsx")

# Residuals for each survey
moran_top_model_all_surveys <- data.frame(pond = df$pond, 
                              year = df$year, 
                              residuals = residuals(TopModelNoAzolla)) |>
  left_join(y = pond_coords, 
            by = join_by(pond),
            unmatched = "drop")

write_xlsx(moran_top_model_all_surveys, "Data/Moran Test Data/moran_top_model_all_points.xlsx")


## Plot average model ----------------------------------------------------------
summary(AvgModelNoAzolla)
tidy_model <- augment(AvgModelNoAzolla)


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
  theme_classic(base_size = 8)

akaike_weights

ggsave(plot = akaike_weights,
       filename = "figure_3.jpeg",
       path = "Figures/",
       dpi = 1200,
       width = 190,
       height = 120,
       units = "mm")



ggsave(plot = akaike_weights,
       filename = "figure_3.tiff",
       path = "Figures/Manuscript Figures/",
       dpi = 1200,
       width = 190,
       height = 120,
       units = "mm")

################################################################################





# Data Visualization ###########################################################
sjPlot::plot_model(model = TopModel,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Habitat Variables on SCLTS Larval Density")

sjPlot:: tab_model(TopModel)
