#Relationship between Azolla and water quality variables
library(tidyverse)
library(lme4)
library(rqs)

data <- read_csv("Data/ysi_included.csv") |>
  dplyr::filter(year >= 2023) 
options(scipen = 999)


data$pond <- as.character(data$pond)
PlanktonAzollaModel <- lmer(log_plankton ~ azolla + (1|pond), data = data)
summary(PlanktonAzollaModel)
anova(PlanktonAzollaModel)

#No significant difference in plankton by presence/absence of Azolla


# Check Azolla against pH
pHAzollaModel <- lmer(log_pH ~ azolla + (1|pond), data = data)
summary(pHAzollaModel)
anova(pHAzollaModel)
rsq::rsq.lmm(pHAzollaModel)


# Azolla against nitrates
nitrateAzolla <- lmer(log_nitrates ~ azolla + (1|pond), data = data)
summary(nitrateAzolla)
anova(nitrateAzolla)
rsq::rsq.lmm(nitrateAzolla)


# Azolla against DO
DOazolla <- lmer(log_DO ~ azolla + (1|pond), data = data)
summary(DOazolla)
anova(DOazolla)
rsq::rsq.lmm(DOazolla)





AzollaInteractionModel <- lmer(log_larv_dens ~ azolla + log_DO + azolla:log_DO + 
                                 (1|pond), data = data)
summary(AzollaInteractionModel)
rsq::rsq.lmm(AzollaInteractionModel)
