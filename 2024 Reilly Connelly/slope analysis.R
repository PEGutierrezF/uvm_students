



# --------------------------------------------------------
# 
# Date: Thu Feb 13 2025 11:56:53
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------




library(readxl); library(dplyr); library(ggplot2)
library(patchwork)



data <- read_xlsx('data.xlsx', sheet='final_data')
head(data,6)

# Exclude the last row
data <- data[-nrow(data), ]# (Yule et al. 2010)

data <- data %>%
  mutate(log_num_spec = log(num_spec),
         log_connect = log(connect))

# We use ANCOVA because it allows us to compare the slopes of the relationship 
# between species richness and connectance while controlling for the effect of region. 
# Unlike simply comparing individual regression slopes, ANCOVA accounts for both the 
# differences in the slopes and the intercepts between the regions. This test combines 
# the effects of both the categorical variable (region) and the continuous variable
# (species richness) in a single model, providing a more robust and accurate 
# comparison. By doing this, ANCOVA helps us determine if the relationship between 
# species richness and connectance differs significantly between regions, 
# while controlling for the influence of region itself.

ancova_model <- lm(log_connect ~ log_num_spec * region, data = data)
summary(ancova_model)

# If the interaction term is significant (as it is in your model, 0.00858 **), 
# it means the slopes are significantly different between the regions, and 
# we don't need to rely on comparing confidence intervals manually. 
# The p-value for the interaction term provides the formal statistical test 
# for this difference.

# Residual plots to check assumptions
par(mfrow=c(2,2))
plot(ancova_model)

# Check normality of residuals
hist(residuals(ancova_model), main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue")
qqnorm(residuals(ancova_model))
qqline(residuals(ancova_model))

# Extract residuals
residuals_ancova <- residuals(ancova_model)
# Perform Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals_ancova)
shapiro_test

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



# Slope -------------------------------------------------------------------
# Extracting Slopes from the ANCOVA Model:
# Extracting coefficients from the ANCOVA model
coefficients_ancova <- coef(ancova_model)

# Slope for temperate region
slope_temperate <- coefficients_ancova["log_num_spec"]
cat("Slope for Temperate Region:", slope_temperate, "\n")

# Slope for tropical region (sum of log_num_spec and log_num_spec:regiontropical)
slope_tropical <- coefficients_ancova["log_num_spec"] + coefficients_ancova["log_num_spec:regiontropical"]
cat("Slope for Tropical Region:", slope_tropical, "\n")


# Summary of the Slopes:
# Temperate Region:
  Slope = -0.3014
# Tropical Region:
  Slope = -0.3014 + (-0.4190) = -0.7203

# Linear mixed model ------------------------------------------------------
# This code fits a linear mixed model (LMM) to analyze how species number (num_spec) 
# and region affect connectance (connect), while accounting for variability across 
# stream orders as a random effect. 

library(lme4)
model <- lmer(connect ~ num_spec * region + (1 | stream_order), data = data)
summary(model)

# The model shows that stream order contributes very little variance (0.0002), 
# meaning its random effect is minimal. The fixed effects suggest that species 
# number (num_spec) has a weak negative effect on connectance, while tropical 
# regions (regiontropical) have a small, non-significant positive effect. 
# However, the interaction (num_spec:regiontropical) is significant (p ≈ 0.03), 
# suggesting that the effect of species number on connectance differs by region. 
# Overall, species richness and region impact connectance, but stream order has a 
# negligible influence.

ggplot(data, aes(x = as.factor(stream_order), y = connect, color = region)) +
  geom_boxplot() +
  theme_minimal()



# Coefficient of variation ------------------------------------------------
# Compare Coefficients of Variation (CV)
cv_temperate <- sd(data$connect[data$region == "temperate"]) / mean(data$connect[data$region == "temperate"])
cv_tropical <- sd(data$connect[data$region == "tropical"]) / mean(data$connect[data$region == "tropical"])

cat("CV Temperate:", cv_temperate, "\n")
cat("CV Tropical:", cv_tropical, "\n")

# Test if Variability in Connectance Differs Between Regions
var.test(connect ~ region, data = data)






