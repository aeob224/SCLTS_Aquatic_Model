#Trying GAMs to model SCLTS larval recruitment
library(tidyverse)
library(mgcv)

data <- read_csv("model_data_no_ysi_or_plankton.csv")
data$vert_pred <- as.character(data$vert_pred)
data$pond <- as.factor(data$pond)
data$year <- as.character(data$year)
data$azolla_presence_absence <- as.character(data$azolla_presence_absence)

#This generates a GAM and because we set select = TRUE it also institutes a double penalty against all smooths
model_all_smooth <- gam(log_larv_dens ~ s(depth) + s(suitable_598_split) + s(suitable_598) + vert_pred + s(log_large_prey) +
                      s(log_invert_pred) + s(log_water_temp) + s(log_salinity) + s(log_turbidity) + s(log_dist_to_breed) + 
                        s(sqrt_chlorophyll) + s(sqrt_emergent_veg) + azolla_presence_absence + s(pond, bs = 're'),
                      data = data, select = TRUE)
summary(model_all_smooth)
plot(model_all_smooth)

#GAM of just the most important variables from mixed effects model selection process
model_important_variables <- gam(log_larv_dens ~ azolla_presence_absence + s(depth) + s(log_dist_to_breed) +
                                   s(log_salinity) +  s(sqrt_emergent_veg) + vert_pred +  s(pond, bs = 're'),
                                 data = data, method = "REML")

summary(model_important_variables)
plot(model_important_variables)







# Depth GAM with dry ponds included.