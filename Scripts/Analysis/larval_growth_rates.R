################################################################################
# Examining Larval Growth Rates
################################################################################
#
# Aidan O'Brien
# aeo83@miami.edu
# 2/28/2026
#
# Description
#

################################################################################
# Load Packages and Data
################################################################################
library(tidyverse)
library(readxl)
options(scipen = 99)

surveys_2023 <- read_xlsx("Data/larvae_2023.xlsx", sheet = 1) |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl, mass)


surveys_2024 <- read_xlsx("Data/larvae_2024.xlsx", sheet = 1) |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl, mass)


upper_2023 <-  read_xlsx("Data/larvae_2023.xlsx", sheet = 1) |>
  filter(pond == "Upper Cattail") |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl)


seascape3_2023 <- read_xlsx("Data/larvae_2023.xlsx", sheet = 1) |>
  filter(pond == "Seascape 3") |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl)


seascape1_2023 <- read_xlsx("Data/larvae_2023.xlsx", sheet = 1) |>
  filter(pond == "Seascape 1") |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl)


calabasas_2023 <- read_xlsx("Data/larvae_2023.xlsx", sheet = 1) |>
  filter(pond == "Calabasas") |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl)
  
ellicott_2024 <- read_xlsx("Data/larvae_2024.xlsx", sheet = 1) |>
  filter(pond == "Ellicott") |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl)

prospect_2024 <- read_xlsx("Data/larvae_2024.xlsx", sheet = 1) |>
  filter(pond == "Prospect") |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl)


calabasas_2024 <- read_xlsx("Data/larvae_2024.xlsx", sheet = 1) |>
  filter(pond == "Calabasas") |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl)


seascape3_2024 <- read_xlsx("Data/larvae_2024.xlsx", sheet = 1) |>
  filter(pond == "Seascape 3") |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl)

upper_2024 <- read_xlsx("Data/larvae_2024.xlsx", sheet = 1) |>
  filter(pond == "Upper Cattail") |>
  mutate(svl = as.numeric(svl),
         mass = as.numeric(mass),
         date = as.Date(date)) |>
  drop_na(svl)

################################################################################
# Plot svl growth over time
################################################################################

# All Surveys 2023 -------------------------------------------------------------
growth_model_2023 <- lm(svl ~ date, data = surveys_2023)
summary(growth_model_2023)
shapiro.test(residuals(growth_model_2023))
qqnorm(y = resid(growth_model_2023))
qqline(y = resid(growth_model_2023))

ggplot(surveys_2023,
       mapping = aes(x = date,
                     y = svl)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.1) +
  theme_classic()


# All Surveys 2024 -------------------------------------------------------------
growth_model_2024 <- lm(svl ~ date, data = surveys_2024)
summary(growth_model_2024)
shapiro.test(residuals(growth_model_2024))
qqnorm(y = resid(growth_model_2024))
qqline(y = resid(growth_model_2024))

ggplot(surveys_2024,
       mapping = aes(x = date,
                     y = svl)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.1) +
  theme_classic()


# Calabasas 2023 ---------------------------------------------------------------
growth_model_calabasas_2023 <- lm(svl ~ date, data = calabasas_2023)
summary(growth_model_calabasas_2023)
shapiro.test(residuals(growth_model_calabasas_2023))
qqnorm(y = resid(growth_model_calabasas_2023))
qqline(y = resid(growth_model_calabasas_2023))

ggplot(calabasas_2023,
       mapping = aes(x = date,
                     y = svl)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()


# Upper Cattail 2023 -----------------------------------------------------------
growth_model_cattail_2023 <- lm(svl ~ date, data = upper_2023)
summary(growth_model_cattail_2023)
shapiro.test(residuals(growth_model_cattail_2023))
qqnorm(y = resid(growth_model_cattail_2023))
qqline(y = resid(growth_model_cattail_2023))

ggplot(upper_2023,
       mapping = aes(x = date,
                     y = svl)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()


# Seascape 3 2023 --------------------------------------------------------------
growth_model_seascape3_2023 <- lm(svl ~ date, data = seascape3_2023)
summary(growth_model_seascape3_2023)
shapiro.test(residuals(growth_model_seascape3_2023))
qqnorm(y = resid(growth_model_seascape3_2023))
qqline(y = resid(growth_model_seascape3_2023))

ggplot(seascape3_2023,
       mapping = aes(x = date,
                     y = svl)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()


# Seascape 1 2023 --------------------------------------------------------------
growth_model_seascape1_2023 <- lm(svl ~ date, data = seascape1_2023)
summary(growth_model_seascape1_2023)
shapiro.test(residuals(growth_model_seascape1_2023))
qqnorm(y = resid(growth_model_seascape1_2023))
qqline(y = resid(growth_model_seascape1_2023))

ggplot(seascape1_2023,
       mapping = aes(x = date,
                     y = svl)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()

library(mgcv)

# Ellicott 2024 ----------------------------------------------------------------
growth_model_ellicott_2024 <- lm(svl ~ date, data = ellicott_2024)
summary(growth_model_ellicott_2024)
shapiro.test(residuals(growth_model_ellicott_2024))

ggplot(ellicott_2024,
       mapping = aes(x = date,
                     y = svl)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()


# Prospect 2024 ----------------------------------------------------------------
growth_model_prospect_2024 <- lm(svl ~ date, data = prospect_2024)
summary(growth_model_prospect_2024)
shapiro.test(residuals(growth_model_prospect_2024))

ggplot(prospect_2024,
       mapping = aes(x = date,
                     y = svl)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()


# Calabasas 2024 ----------------------------------------------------------------
growth_model_calabasas_2024 <- lm(svl ~ date, data = calabasas_2024)
summary(growth_model_calabasas_2024)
shapiro.test(residuals(growth_model_calabasas_2024))

ggplot(calabasas_2024,
       mapping = aes(x = date,
                     y = svl)) +
  geom_jitter(width = 0.5) +
  stat_smooth(method = "lm") +
  theme_classic()


# Seascape 3 2024 --------------------------------------------------------------
growth_model_seascape3_2024 <- lm(svl ~ date, data = seascape3_2024)
summary(growth_model_seascape3_2024)
shapiro.test(residuals(growth_model_seascape3_2024))

ggplot(seascape3_2024,
       mapping = aes(x = date,
                     y = svl)) +
  geom_jitter(width = 0.5) +
  stat_smooth(method = "lm") +
  theme_classic()






################################################################################
# Plot mass growth over time
################################################################################

# All Surveys 2023 -------------------------------------------------------------
mass_model_2023 <- lm(mass ~ date, data = surveys_2023)
summary(mass_model_2023)
shapiro.test(residuals(mass_model_2023))
qqnorm(y = resid(mass_model_2023))
qqline(y = resid(mass_model_2023))

ggplot(surveys_2023,
       mapping = aes(x = date,
                     y = mass)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.1) +
  theme_classic()


# All Surveys 2024 -------------------------------------------------------------
mass_model_2024 <- lm(mass ~ date, data = surveys_2024)
summary(mass_model_2024)
shapiro.test(residuals(mass_model_2024))
qqnorm(y = resid(mass_model_2024))
qqline(y = resid(mass_model_2024))

ggplot(surveys_2024,
       mapping = aes(x = date,
                     y = mass)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.1) +
  theme_classic()


# Calabasas 2023 ---------------------------------------------------------------
mass_model_calabasas_2023 <- lm(mass ~ date, data = calabasas_2023)
summary(mass_model_calabasas_2023)
shapiro.test(residuals(mass_model_calabasas_2023))

ggplot(calabasas_2023,
       mapping = aes(x = date,
                     y = mass)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()


# Upper Cattail 2023 -----------------------------------------------------------
mass_model_cattail_2023 <- lm(mass ~ date, data = upper_2023)
summary(mass_model_cattail_2023)
shapiro.test(residuals(mass_model_cattail_2023))
qqnorm(y = resid(mass_model_cattail_2023))
qqline(y = resid(mass_model_cattail_2023))

ggplot(upper_2023,
       mapping = aes(x = date,
                     y = mass)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()


# Seascape 3 2023 --------------------------------------------------------------
mass_model_seascape3_2023 <- lm(mass ~ date, data = seascape3_2023)
summary(mass_model_seascape3_2023)
shapiro.test(residuals(mass_model_seascape3_2023))
qqnorm(y = resid(mass_model_seascape3_2023))
qqline(y = resid(mass_model_seascape3_2023))

ggplot(seascape3_2023,
       mapping = aes(x = date,
                     y = mass)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()


# Seascape 1 2023 --------------------------------------------------------------
mass_model_seascape1_2023 <- lm(mass ~ date, data = seascape1_2023)
summary(mass_model_seascape1_2023)
shapiro.test(residuals(mass_model_seascape1_2023))
qqnorm(y = resid(mass_model_seascape1_2023))
qqline(y = resid(mass_model_seascape1_2023))

ggplot(seascape1_2023,
       mapping = aes(x = date,
                     y = mass)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()


# Ellicott 2024 ----------------------------------------------------------------
mass_model_ellicott_2024 <- lm(mass ~ date, data = ellicott_2024)
summary(mass_model_ellicott_2024)
shapiro.test(residuals(mass_model_ellicott_2024))

ggplot(ellicott_2024,
       mapping = aes(x = date,
                     y = mass)) +
  stat_smooth(method = "lm") +
  geom_jitter(width = 0.5) +
  theme_classic()


# Prospect 2024 ----------------------------------------------------------------
mass_model_prospect_2024 <- lm(mass ~ date, data = prospect_2024)
summary(mass_model_prospect_2024)
shapiro.test(residuals(mass_model_prospect_2024))

ggplot(prospect_2024,
       mapping = aes(x = date,
                     y = mass)) +
    geom_smooth()+
  geom_jitter(width = 0.5) +
  theme_classic()


# Calabasas 2024 ----------------------------------------------------------------
mass_model_calabasas_2024 <- lm(log(mass) ~ date, data = calabasas_2024)
summary(mass_model_calabasas_2024)
shapiro.test(residuals(mass_model_calabasas_2024))
qqnorm(y = resid(mass_model_calabasas_2024))
qqline(y = resid(mass_model_calabasas_2024))

ggplot(calabasas_2024,
       mapping = aes(x = date,
                     y = log(mass))) +
  geom_jitter(width = 0.5) +
  stat_smooth(method = "lm") +
  theme_classic()


# Seascape 3 2024 --------------------------------------------------------------
mass_model_seascape3_2024 <- lm(mass ~ date, data = seascape3_2024)
summary(mass_model_seascape3_2024)
shapiro.test(residuals(mass_model_seascape3_2024))

ggplot(seascape3_2024,
       mapping = aes(x = date,
                     y = mass)) +
  geom_point() +
  geom_jitter(width = 0.5) +
  stat_smooth(method = "lm") +
  theme_classic()
