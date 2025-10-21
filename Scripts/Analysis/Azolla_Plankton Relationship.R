#Relationship between Azolla and Plankton
library(tidyverse)
library(lme4)
data <- read_csv("Data/model_data_no_ysi.csv")

data$pond <- as.character(data$pond)
PlanktonAzollaModel <- lmer(log_plankton ~ azolla_presence_absence + (1|pond), data = data)
summary(PlanktonAzollaModel)
anova(PlanktonAzollaModel)

#No significant difference in plankton by presence/absence of Azolla