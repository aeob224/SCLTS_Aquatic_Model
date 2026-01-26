#Univariate Fits of model data

#Load Packages
library(lme4)
library(lmerTest)
library(tidyverse)
library(broom.mixed)
library(cowplot)


################################################################################
# Read Data 
################################################################################
data <- read_csv("Data/univariate_analysis.csv") |>
  mutate(azolla = as.factor(azolla))


################################################################################
# Run univariate models
################################################################################

# Linear fit function generation -----------------------------------------------
uni_model <- function(predictor, data) {
  as.numeric(predictor)
  model <- lmerTest::lmer(log_larv_dens ~ predictor + (1|pond), data = data, na.action = na.exclude)
  print(summary(model))
  anova(model)
}

# Run Models -------------------------------------------------------------------

# Azolla (p = 0.008)**
uni_model(data$azolla, data)

# Depth  (p = 0.093)
uni_model(data$depth, dat = data)

# Distance to nearest breeding pond (p = 0.004)**
uni_model(data$log_dist_to_breed, dat = data)

# Emergent vegetation (p = 0.064)
uni_model(data$sqrt_emergent_veg, dat = data)

# Medium Prey (p = 0.064)
uni_model(data$log_med_prey, dat = data)

# Large Prey (p = 0.862)
uni_model(data$log_large_prey, dat = data)

# Invert Pred (p = 0.654)
uni_model(data$log_invert_pred, dat = data)

# Plankton (p = 0.473)
uni_model(data$log_plankton, dat = data)

# Water Temp (p = 0.974)
uni_model(data$log_water_temp, dat = data)

# DO (p = 0.143)
uni_model(data$log_DO, dat = data)

# Nitrates (p = 0.376)
uni_model(data$log_nitrates, dat = data)

# pH (p = 0.775)
uni_model(data$log_pH, dat = data)

# Salinity (p = 0.018)*
uni_model(data$log_salinity, dat = data)

# Turbidity (p = 0.559)
uni_model(data$log_turbidity, dat = data)

# Chlorophyll (p = 0.652)
uni_model(data$sqrt_chlorophyll, dat = data)

#Suitable 598 (p = 0.325)
uni_model(data$suitable_598, dat = data)


#Suitable 598 split (p = 0.400)
uni_model(data$suitable_598_split, dat = data)

################################################################################




################################################################################
# Quadratic Models
################################################################################

# Quadratic Model function generation ------------------------------------------
quad_model <- function(predictor, data) {
  model <- lmer(log_larv_dens ~ poly(predictor,2) + (1|pond), data = data, na.action = na.exclude)
  summary(model)
}


# Depth
dat <- data |>
  select(depth, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$depth, dat)


# Distance to nearest breeding pond
dat <- data |>
  dplyr::select(log_dist_to_breed, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_dist_to_breed, dat)


# Emergent vegetation
dat <- data |>
  dplyr::select(sqrt_emergent_veg, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$sqrt_emergent_veg, dat)


# Medium Prey
dat <- data |>
  dplyr::select(log_med_prey, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_med_prey, dat)


# Large Prey
dat <- data |>
  dplyr::select(log_large_prey, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_large_prey, dat)


# Invert Pred
dat <- data |>
  dplyr::select(log_invert_pred, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_invert_pred, dat)


# Plankton
dat <- data |>
  dplyr::select(log_plankton, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_plankton, dat)


# Water Temp
dat <- data |>
  dplyr::select(log_water_temp, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_water_temp, dat)


# DO
dat <- data |>
  dplyr::select(log_DO, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_DO, dat)


# Nitrates
dat <- data |>
  dplyr::select(log_nitrates, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_nitrates, dat)


# pH
dat <- data |>
  dplyr::select(log_pH, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_pH, dat)


# Salinity
dat <- data |>
  dplyr::select(log_salinity, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_salinity, dat)


# Turbidity
dat <- data |>
  dplyr::select(log_turbidity, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_turbidity, dat)


# Chlorophyll
dat <- data |>
  dplyr::select(sqrt_chlorophyll, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$sqrt_chlorophyll, dat)




################################################################################
# Visualizing the three significant bivariate fits 
################################################################################

## Distance Model --------------------------------------------------------------
distance_model <- lmer(log_larv_dens ~ log_dist_to_breed +  (1|pond), data = data)
summary(distance_model)

tidy_distance <- augment(distance_model)

### This plots the actual datapoints but fits a line to the predicted effects
distance_plot <- ggplot(data = tidy_distance,
       mapping = aes(x = log_dist_to_breed,
                     y = .fitted))+
  geom_smooth(method = "lm", level = 0.95)+
  geom_point(mapping = aes(x = log_dist_to_breed,
                           y = log_larv_dens)) +
  labs(x = "Distance to Nearest Breeding Pond (log m)",
       y = "Predicted log-Larval Density (log larvae/ " ~m^2) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        title = element_text(size = 20))

distance_plot

##Salinity Model ---------------------------------------------------------------
salinity_model <- lmer(log_larv_dens ~ log_salinity +  (1|pond), data = data)
summary(salinity_model)
tidy_salinity <- augment(salinity_model)

### This plots the actual datapoints but fits a line to the predicted response
salinity_plot <- ggplot(data = tidy_salinity,
       mapping = aes(x = log_salinity,
                     y = .fitted))+
  geom_smooth(method = "lm", level = 0.95)+
  geom_point(mapping = aes(x = log_salinity,
                           y = log_larv_dens)) +
  labs(x = "Salinity (log ppt)",
       y = "Predicted log-Larval Density (log larvae/ " ~m^2) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        title = element_text(size = 20))
 
salinity_plot


## Azolla model ----------------------------------------------------------------
levels(data$azolla) <- c('Absent', 'Present')
azolla_model <- lmer(log_larv_dens ~ azolla +  (1|pond), data = data, na.action = na.exclude)
summary(azolla_model)

tidy_azolla <- augment(azolla_model)

## Boxplot of azolla model predictions. Still uses fits rather than fixed.
## Note, right now the points being displayed are predictions rather than the actual values
azolla_plot <- ggplot(data = tidy_azolla, aes(x = azolla, y = .fitted)) + 
  geom_boxplot() +
  geom_jitter(mapping = aes(x = azolla, y = log_larv_dens), width = 0.1) +
  theme_classic()+
  labs(x = "Azolla",
       y = "log-Predicted Larval Density (log larvae/ " ~m^2) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        title = element_text(size = 20))

azolla_plot

multiPlot <- cowplot::plot_grid(azolla_plot, distance_plot, salinity_plot,
                                nrow = 1,
                                labels = c("A", "B", "C"),
                                label_size = 30)
multiPlot
ggsave("Figures/bivariate_fits.png", multiPlot, width = 25, height = 10)

