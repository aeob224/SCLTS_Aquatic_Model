#Making nice correlation plots for recruitment correlation
library(ggplot2)
library(tidyverse)
library(ggpubr)
df <- read_csv("Data/Larval_Correlations_Sum_Density.csv")

#Subsetting the data by year
data2021 <- df %>%
  filter(Year == 2021) %>%
  dplyr::select(Pond, Metamorph_Estimate, Drift_Fence_Actual_Count)

data2022 <- df %>%
  filter(Year == 2022) %>%
  dplyr::select(Pond, Metamorph_Estimate, Drift_Fence_Actual_Count)

data2023 <- df %>%
  filter(Year == 2023) %>%
  dplyr::select(Pond, Metamorph_Estimate, Drift_Fence_Actual_Count)

data2024 <- df %>%
  filter(Year == 2024) %>%
  dplyr::select(Pond, Metamorph_Estimate, Drift_Fence_Actual_Count)


#2021 Model
model2021 <- lm(log(Drift_Fence_Actual_Count+1) ~ log(Metamorph_Estimate+1), data = data2021)
summary(model2021)

#2022 Model
model2022 <- lm(log(Drift_Fence_Actual_Count+1) ~ log(Metamorph_Estimate+1), data = data2022)
summary(model2022)

#2023 Model
model2023 <- lm(log(Drift_Fence_Actual_Count+1) ~ log(Metamorph_Estimate+1), data = data2023)
summary(model2023)

#2024 Model
model2024 <- lm(log(Drift_Fence_Actual_Count) ~ log(Metamorph_Estimate+1), data = data2024)
summary(model2024)

summary2021 <- summary(model2021)
summary2022 <- summary(model2022)
summary2023 <- summary(model2023)
summary2024 <- summary(model2024)

p_value2021 <- coef(summary(model2021))[2, 4]
p_value2022 <- coef(summary(model2022))[2, 4]
p_value2023 <- coef(summary(model2023))[2, 4]
p_value2024 <- coef(summary(model2024))[2, 4]




#2021 Plot
plot1 <- ggplot(data2021, aes(x = log(Metamorph_Estimate+1), y = log(Drift_Fence_Actual_Count+1))) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", color = "black")+
  labs(x = "Predicted Metamorphs (log count)",
       y = "Observed Metamorphs (log count)",
       title = "2021")+
  theme_classic() +
  theme(plot.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18)) +
  annotate("text", x = 0.3, y = 2.6, label = paste("R² = ", round(summary2021$r.squared, 3), "\n", "p = ", round(p_value2021, 3)),
           size = 5, color = "black")
plot1


#2022 Plot
plot2 <- ggplot(data2022, aes(x = log(Metamorph_Estimate+1), y = log(Drift_Fence_Actual_Count+1))) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", color = "black")+
  labs(x = "Predicted Metamorphs (log count)",
       y = "Observed Metamorphs (log count)",
       title = "2022")+
  theme_classic() +
  theme(plot.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18)) +
  annotate("text", x = 0.3, y = 5.5, label = paste("R² = ", round(summary2022$r.squared, 3), "\n", "p = ", round(p_value2022, 3)),
           size = 5, color = "black")

plot2

#2023 Plot
plot3 <- ggplot(data2023, aes(x = log(Metamorph_Estimate+1), y = log(Drift_Fence_Actual_Count+1))) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", color = "black")+
  labs(x = "Predicted Metamorphs (log count)",
       y = "Observed Metamorphs (log count)",
       title = "2023")+
  theme_classic() +
  theme(plot.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18)) +
  annotate("text",x = 0.25, y = 5.4, label = paste("R² = ", round(summary2023$r.squared, 3), "\n", "p = ", round(p_value2023, 3)),
           size = 5, color = "black")
plot3

#2024 Plot
plot4 <- ggplot(data2024, aes(x = log(Metamorph_Estimate+1), y = log(Drift_Fence_Actual_Count))) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", colour = "black")+
  labs(x = "Predicted Metamorphs (log count)",
       y = "Observed Metamorphs (log count)",
       title = "2024")+
  theme_classic() +
  theme(plot.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18))+
  annotate("text",  x = 0.25, y = 6.7, label = paste("R² = ", round(summary2024$r.squared, 3), "\n", "p = ", round(p_value2024, 3)),
           size = 5, color = "black") 
plot4



#2022 without Calabasas
data2022NoCalabasas <- df %>%
  filter(Year == 2022, Pond != "Calabasas") %>%
  dplyr::select(Pond, Metamorph_Estimate, Drift_Fence_Actual_Count)


model2022NoCalabasas <- lm(log(Drift_Fence_Actual_Count+1) ~ log(Metamorph_Estimate+1), data = data2022NoCalabasas)
summary(model2022NoCalabasas)

summary2022NoCalabasas <- summary(model2022NoCalabasas)
p_value2022NoCalabasas <- coef(summary(model2022NoCalabasas))[2, 4]



plot5 <- ggplot(data2022NoCalabasas, aes(x = log(Metamorph_Estimate+1), y = log(Drift_Fence_Actual_Count+1))) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", color = "black")+
  labs(x = "Predicted Metamorphs (log count)",
       y = "Observed Metamorphs (log count)",
       title = "2022 without Calabasas")+
  theme_classic() +
  theme(plot.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18)) +
  annotate("text", 
           x = 0.3, 
           y = 6.5, 
           label = paste("R² = ", round(summary2022NoCalabasas$r.squared, 3), "\n", "p = ", round(p_value2022NoCalabasas, 3)),
           size = 5, 
           color = "black")
plot5


multiPlot <- cowplot::plot_grid(plot1, plot2, plot3, plot4, plot5,
                                nrow = 2,
                                ncol = 3,
                                labels = c("A", "B", "C", "D", "E"),
                                label_size = 20)
multiPlot
ggsave("Figures/drift_fence_correlations.jpg", multiPlot, width = 25, height = 15)



#Now assessing all years pooled
all_years <- lm(log(Drift_Fence_Actual_Count+1) ~ log(Metamorph_Estimate+1), data = df)
summary(all_years)

##Returning the slope of the regression to calculate trespass rate
all_years_slope <- coef(all_years)[2]
##Slope is 0.24











