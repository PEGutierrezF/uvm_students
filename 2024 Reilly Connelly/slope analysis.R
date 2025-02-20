



# --------------------------------------------------------
# 
# Date: Thu Feb 13 2025 11:56:53
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------

library(boot)

data <- read_xlsx('data.xlsx', sheet='final_data')
head(data,6)

data <- data %>%
  mutate(log_num_spec = log(num_spec),
         log_connect = log(connect))

ancova_model <- lm(log_connect ~ log_num_spec * region, data = data)
summary(ancova_model)
confint(ancova_model)


# Plot log-log relationship with regression lines and confidence intervals
ggplot(data, aes(x = log_num_spec, y = log_connect, color = region)) +
  geom_point(alpha = 0.6, size = 3) +  # Add points with transparency
  geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, alpha = 0.2) +  # Regression lines with confidence intervals
  scale_color_manual(values = c("tropical" = "#E69F00", "temperate" = "#0072B2")) +  # Custom colors
  labs(x = "Log(Number of Species)", 
       y = "Log(Connectivity)", 
       title = "Comparison of Slopes Between Tropical and Temperate Regions",
       color = "Region") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 14))


# Extract the confidence intervals for the ANCOVA model
confint_ancova <- confint(ancova_model)

# Extract the specific confidence intervals for the slopes
confint_slope_temperate <- confint_ancova["log_num_spec", ]
confint_slope_tropical <- confint_ancova["log_num_spec:regiontropical", ]

# Print the results
cat("Confidence Interval for Slope (Temperate):", confint_slope_temperate, "\n")
cat("Confidence Interval for Slope (Tropical):", confint_slope_tropical, "\n")

# Compare the confidence intervals
if (confint_slope_tropical[2] < confint_slope_temperate[1] || confint_slope_temperate[2] < confint_slope_tropical[1]) {
  cat("Slopes are significantly different.\n")
} else {
  cat("Slopes are not significantly different.\n")
}


# Prepare a data frame with slopes and their confidence intervals
slopes_data <- data.frame(
  Region = c("Temperate", "Tropical"),
  Slope = c(coef(ancova_model)["log_num_spec"], coef(ancova_model)["log_num_spec:regiontropical"]),
  Lower_CI = c(confint_ancova["log_num_spec", 1], confint_ancova["log_num_spec:regiontropical", 1]),
  Upper_CI = c(confint_ancova["log_num_spec", 2], confint_ancova["log_num_spec:regiontropical", 2])
)

# Plot the slopes and their confidence intervals
ggplot(slopes_data, aes(x = Region, y = Slope, color = Region)) +
  geom_point(size = 4) +  # Plot the estimated slope
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, linewidth = 1) +  # Add the error bars (confidence intervals)
  scale_color_manual(values = c("Tropical" = "#E69F00", "Temperate" = "#0072B2")) +  # Custom colors
  theme_minimal() +
  labs(title = "Slopes of the Log-Log Relationship Between Links and Species Richness",
       y = "Slope Estimate",
       x = "Region",
       color = "Region") +
  theme(legend.position = "none")




library(lme4)
model <- lmer(connect ~ num_spec * region + (1 | stream_order), data = data)
summary(model)

ggplot(data, aes(x = as.factor(stream_order), y = connect, color = region)) +
  geom_boxplot() +
  theme_minimal()

# Compare Coefficients of Variation (CV)
cv_temperate <- sd(data$connect[data$region == "temperate"]) / mean(data$connect[data$region == "temperate"])
cv_tropical <- sd(data$connect[data$region == "tropical"]) / mean(data$connect[data$region == "tropical"])

cat("CV Temperate:", cv_temperate, "\n")
cat("CV Tropical:", cv_tropical, "\n")

# Test if Variability in Connectance Differs Between Regions
var.test(connect ~ region, data = data)






