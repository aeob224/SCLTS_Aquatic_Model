#Univariate Fits of model data

#Load Packages
library(lme4)
library(lmerTest)
library(tidyverse)
library(broom.mixed)
library(cowplot)
library(AICcmodavg)

################################################################################
# Read Data 
################################################################################
data <- read_csv("Data/univariate_data.csv") |>
  mutate(azolla = as.factor(azolla),
         vert_pred = as.factor(vert_pred),
         pond = as.factor(pond))

dry_pond_data <- read_csv("Data/dry_pond_dataset.csv") |>
  mutate(pond = as.factor(pond))


################################################################################
# Run univariate models
################################################################################

# Linear fit function generation -----------------------------------------------
uni_model <- function(predictor, data) {
  as.numeric(predictor)
  model <- lmerTest::lmer(log_larv_dens ~ predictor + (1|pond), data = data, na.action = na.exclude, REML = F)
  print(summary(model))
  print(AICc(model))
  anova(model)
  
}

# Run Models -------------------------------------------------------------------

# Azolla (p = 0.011)**
uni_model(data$azolla, data)


# Depth  (p = 0.141)
uni_model(dry_pond_data$depth, dry_pond_data)

# Distance to nearest breeding pond (p = 0.02)**
uni_model(data$log_dist_to_breed, dat = data)

# Emergent vegetation (p = 0.031) **
uni_model(data$sqr_emergent_veg, dat = data)

# Medium Prey (p = 0.065)
uni_model(data$log_med_prey, dat = data)

# Large Prey (p = 0.694)
uni_model(data$log_large_prey, dat = data)

# Invert Pred (p = 0.712)
uni_model(data$log_invert_pred, dat = data)

# Plankton (p = 0.459)
uni_model(data$log_plankton, dat = data)

# Water Temp (p = 0.988)
uni_model(data$log_water_temp, dat = data)

# DO (p = 0.093)
uni_model(data$log_DO, dat = data)

# Nitrates (p = 0.45)
uni_model(data$log_nitrates, dat = data)

# pH (p = 0.642)
uni_model(data$log_pH, dat = data)

# Salinity (p = 0.013)*
uni_model(data$log_salinity, dat = data)

# Turbidity (p = 0.906)
uni_model(data$log_turbidity, dat = data)

# Chlorophyll (p = 0.693)
uni_model(data$sqrt_chlorophyll, dat = data)

#Suitable 598 (p = 0.110)
uni_model(data$suitable_598, dat = data)

#Suitable 598 split (p = 0.136)
uni_model(data$suitable_598_split, dat = data)

################################################################################




################################################################################
# Quadratic Models
################################################################################

# Quadratic Model function generation ------------------------------------------
quad_model <- function(predictor, data) {
  model <- lmer(log_larv_dens ~ poly(predictor,2) + (1|pond), data = data, na.action = na.exclude, REML = F)
  print(summary(model))
  print(AICc(model))
}


# Depth (AICc lower than linear)
dat <- dry_pond_data |>
  dplyr::select(depth, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$depth, dat)


# Distance to nearest breeding pond
dat <- data |>
  dplyr::select(log_dist_to_breed, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$log_dist_to_breed, dat)


# Emergent vegetation
dat <- data |>
  dplyr::select(sqr_emergent_veg, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$sqr_emergent_veg, dat)


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

# Suitable Hab 598m
dat <- data |>
  dplyr::select(suitable_598, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$suitable_598, dat)

# Suitable Hab 598m Road Split
dat <- data |>
  dplyr::select(suitable_598_split, log_larv_dens, pond) |>
  drop_na()

quad_model(dat$suitable_598_split, dat)


# Plot Significant Quadratics --------------------------------------------------
## Depth
depth_model <- lmer(log_larv_dens ~ poly(depth,2) + (1|pond), 
                    data = dry_pond_data, 
                    na.action = na.exclude)
tidy_depth <- augment(depth_model)
tidy_depth$depth <- dry_pond_data$depth

ggplot(tidy_depth, aes(x = depth, y = .fitted)) +
      geom_point(aes(x = depth, y = log_larv_dens)) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  theme_classic()

## Large Prey ------------------------------------------------------------------
large_prey_dat <- data |>
  dplyr::select(log_large_prey, log_larv_dens, pond) |>
  drop_na()

large_prey_model <- lmer(log_larv_dens ~ poly(log_large_prey,2) + (1|pond), 
                    data = large_prey_dat, 
                    na.action = na.exclude)
tidy_prey <- augment(large_prey_model)
tidy_prey$prey <- large_prey_dat$log_large_prey

ggplot(tidy_prey, aes(x = prey, y = .fitted)) +
      geom_point(aes(x = prey, y = log_larv_dens)) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  theme_classic()

# Water Temperature -----------------------------------------------------------
water_dat <- data |>
  dplyr::select(log_water_temp, log_larv_dens, pond) |>
  drop_na()

temp_model <- lmer(log_larv_dens ~ poly(log_water_temp,2) + (1|pond), 
                    data = water_dat, 
                    na.action = na.exclude)
tidy_temp <- augment(temp_model)
tidy_temp$temp <- water_dat$log_water_temp

ggplot(tidy_temp, aes(x = temp, y = .fitted)) +
      geom_point(aes(x = temp, y = log_larv_dens)) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  theme_classic()

# Note: We left quadratics out of our assessmen


################################################################################
# Visualizing the four significant bivariate fits 
################################################################################

## Distance Model --------------------------------------------------------------
distance_model <- lmer(log_larv_dens ~ log_dist_to_breed +  (1|pond), data = data)
summary(distance_model)
p_valueDist <- coef(summary(distance_model))[2,5]

tidy_distance <- augment(distance_model)

### This plots the actual datapoints but fits a line to the predicted effects
distance_plot <- ggplot(data = tidy_distance,
       mapping = aes(x = log_dist_to_breed,
                     y = .fitted))+
  geom_smooth(method = "lm", level = 0.95, color = "black")+
  geom_point(mapping = aes(x = log_dist_to_breed,
                           y = log_larv_dens)) +
  labs(x = "Distance to Nearest Breeding Pond (log m)",
       y = "Larval Density (log larvae/"~m^2~")") +
  theme_classic() +
  theme(axis.title = element_text(size = 34),
        axis.text = element_text(size = 34),
        title = element_text(size = 20)) +
  annotate("text", x = 1.6, y = 0.55, label = paste("p = ", round(p_valueDist, 3)),
           size = 18, color = "black") 

distance_plot
ggsave("Figures/distance.jpg", distance_plot, width = 25, height = 15)

##Salinity Model ---------------------------------------------------------------
salinity_model <- lmer(log_larv_dens ~ log_salinity +  (1|pond), data = data)
summary(salinity_model)
p_value_salinity <- coef(summary(salinity_model))[2,5]
tidy_salinity <- augment(salinity_model)

### This plots the actual datapoints but fits a line to the predicted response
salinity_plot <- ggplot(data = tidy_salinity,
       mapping = aes(x = log_salinity,
                     y = .fitted))+
  geom_smooth(method = "lm", level = 0.95, color = "black")+
  geom_point(mapping = aes(x = log_salinity,
                           y = log_larv_dens)) +
  labs(x = "Salinity (log ppt)",
       y = "Larval Density (log larvae/"~m^2~")") +
  theme_classic() +
  theme(axis.title = element_text(size = 34),
        axis.text = element_text(size = 34),
        title = element_text(size = 20)) +
  annotate("text", x = 0.8, y = 0.55, label = paste("p = ", round(p_value_salinity, 3)),
           size = 18, color = "black") 


salinity_plot
ggsave("Figures/salinity.jpg", salinity_plot, width = 25, height = 15)


## Azolla model ----------------------------------------------------------------
levels(data$azolla) <- c('Absent', 'Present')
azolla_model <- lmer(log_larv_dens ~ azolla +  (1|pond), data = data, na.action = na.exclude)
p_value_azolla <- coef(summary(azolla_model))[2,5]
summary(azolla_model)

tidy_azolla <- augment(azolla_model)

## Boxplot of azolla model predictions. Still uses fits rather than fixed.
azolla_plot <- ggplot(data = tidy_azolla, aes(x = azolla, y = .fitted)) + 
  geom_boxplot() +
  geom_jitter(mapping = aes(x = azolla, y = log_larv_dens), width = 0.1) +
  theme_classic()+
  labs(x = "Azolla Presence",
       y = "Larval Density (log larvae/"~m^2~")") +
  theme(axis.title = element_text(size = 34),
        axis.text = element_text(size = 34),
        title = element_text(size = 20)) +
  annotate("text", x = 0.6, y = 0.55, label = paste("p = ", round(p_value_azolla, 3)),
           size = 18, color = "black") 


azolla_plot
ggsave("Figures/azolla.jpg", azolla_plot, width = 25, height = 15)

##Vegetation Model ---------------------------------------------------------------
veg_model <- lmer(log_larv_dens ~ sqr_emergent_veg +  (1|pond), data = data, na.action = na.exclude)
summary(salinity_model)
p_value_veg <- coef(summary(veg_model))[2,5]

tidy_veg <- augment(veg_model)

### This plots the actual datapoints but fits a line to the predicted response
veg_plot <- ggplot(data = tidy_veg,
       mapping = aes(x = sqr_emergent_veg,
                     y = .fitted))+
  geom_smooth(method = "lm", level = 0.95, color = "black")+
  geom_point(mapping = aes(x = sqr_emergent_veg,
                           y = log_larv_dens)) +
  labs(x = "Emergent Vegetation Percent Coverage (square root transformed)",
       y = "Larval Density (log larvae/"~m^2~")") +
  theme_classic() +
  theme(axis.title = element_text(size = 34),
        axis.text = element_text(size = 34),
        title = element_text(size = 20)) +
  annotate("text", x = 0.5, y = 0.55, label = paste("p = ", round(p_value_veg, 3)),
           size = 18, color = "black") 

 
veg_plot
ggsave("Figures/emergent_veg.jpg", veg_plot, width = 25, height = 15)



# Saving the multiplot ---------------------------------------------------------

multiPlot <- cowplot::plot_grid(azolla_plot, salinity_plot, distance_plot, veg_plot,
                                nrow = 2,
                                labels = c("A", "B", "C", "D"),
                                label_size = 30)
multiPlot
ggsave("Figures/univariate_fits.png", multiPlot, width = 25, height = 15)
