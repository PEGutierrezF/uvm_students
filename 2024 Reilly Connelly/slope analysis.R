



# --------------------------------------------------------
# 
# Date: Thu Feb 13 2025 11:56:53
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------

library(boot)

data <- read_xlsx('data.xlsx', sheet='update')
head(data,20)

data <- data %>%
  mutate(log_num_spec = log(num_spec),
         log_connect = log(connect))


model_tropical <- lm(log_connect ~ log_num_spec, data = data, subset = region == "tropical")
model_temperate <- lm(log_connect ~ log_num_spec, data = data, subset = region == "temperate")

# Summary of models
summary(model_tropical)
summary(model_temperate)


# Get confidence intervals
confint_tropical <- confint(model_tropical)
confint_temperate <- confint(model_temperate)

# Extract slopes and their confidence intervals
slope_tropical <- coef(model_tropical)[2]
slope_temperate <- coef(model_temperate)[2]

confint_tropical_slope <- confint_tropical[2, ]
confint_temperate_slope <- confint_temperate[2, ]

# Print results
cat("Tropical Slope:", slope_tropical, "95% CI:", confint_tropical_slope, "\n")
cat("Temperate Slope:", slope_temperate, "95% CI:", confint_temperate_slope, "\n")

# Check if confidence intervals overlap
if (confint_tropical_slope[2] < confint_temperate_slope[1] || confint_temperate_slope[2] < confint_tropical_slope[1]) {
  cat("Slopes are significantly different.\n")
} else {
  cat("Slopes are not significantly different.\n")
}


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



# Confidence Interval Plot ------------------------------------------------
# Create a data frame for plotting
slope_data <- data.frame(
  region = c("Tropical", "Temperate"),
  slope = c(-0.7429, -0.3059),
  lower_CI = c(-0.933, -0.501),
  upper_CI = c(-0.553, -0.111)
)


# Plot the slopes with confidence intervals
ggplot(slope_data, aes(x = region, y = slope, color = region)) +
  geom_point(size = 4) +  # Plot the estimated slope
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2, linewidth = 1) +  # Add confidence intervals
  scale_color_manual(values = c("Tropical" = "#E69F00", "Temperate" = "#0072B2")) +  # Custom colors
  labs(x = "", y = "Slope Estimate", title = "Comparison of Slopes Between Regions") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 14))



