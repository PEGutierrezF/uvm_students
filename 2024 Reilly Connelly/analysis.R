



# --------------------------------------------------------
# Honor Thesis Reilly Connelly
# Date: Sat Oct 26 2024 15:49:27
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------




library(readxl); library(dplyr); library(ggplot2)
library(patchwork)




data <- read_xlsx('2024 Reilly Connelly/data.xlsx', sheet='final_data')

# Exclude the last row
data <- data[-nrow(data), ]# (Yule et al. 2010)

head(data,20)


# Number of links vs Species Richness  ------------------------------------
data <- data %>%
  mutate(log_num_spec = log(num_spec),
         log_num_link = log(num_link))

# ANCOVA
ancova_mod_links_log <- lm(log_num_link ~ log_num_spec * region, data = data)
summary(ancova_mod_links_log)

# The significant interaction term (p = 0.000566) shows that species richness 
# affects the number of links differently across regions. Temperate streams have 
# a steeper increase in links (slope = 1.7022), while tropical streams have a 
# slower increase (slope = 1.1655) but start with more links. This suggests 
# temperate streams form interactions more flexibly, whereas tropical streams 
# may have more constrained or specialized networks.


shapiro.test(residuals(ancova_mod_links_log))  # Check normality again


# Summary of the Slopes:
# Temperate Region:
  Slope = log_num_spec coefficient = 1.7022
# Tropical Region:
  Slope = 1.7022 + (-0.5367) = 1.1655


# Create the plot
ggplot(data, aes(x = num_spec, y = num_link, 
                 color = region)) +
  geom_point(size = 3) +  
  labs(
    x = "Connectance",
    y = "Number of Species",
    color = "stream_order"
  ) +
  theme_minimal() +
  theme(legend.position = "top") 



# Create the plot with linear models for both regions on the same plot
a <- ggplot(data, aes(x = num_spec, y = num_link, 
                      color = region)) +
  geom_point(size = 3) +  # Points colored by region
  geom_smooth(method = "lm", se = T) +  # Add separate linear models for each region
  labs(
    x = "Number of Species",
    y = "num_link",
    color = "region"
  ) +
  theme_minimal() +
  theme(legend.position = "top")  # Move the legend to the top
a


# Species richness–connectance relationship -------------------------------
# Create the plot with linear models for both regions on the same plot
# Fit an inverse model for each region
  model_results <- data %>%
  group_by(region) %>%
  do(model = nls(num_spec ~ a / connect + b, data = ., start = list(a = 1, b = 1)))

# Generate predicted values for plotting
predicted_data <- data %>%
  group_by(region) %>%
  do({
    model <- nls(connect ~ a / num_spec + b, data = ., start = list(a = 1, b = 0))
    data.frame(num_spec = seq(min(.$num_spec), max(.$num_spec), length.out = 100),
               connect = predict(model, newdata = data.frame(num_spec = seq(min(.$num_spec), max(.$num_spec), length.out = 100))),
               region = unique(.$region))
  })

# Plot the data and fitted curves
b <- ggplot(data, aes(x = num_spec, y = connect, color = region)) +
  geom_point(size = 3) +  # Data points
  geom_line(data = predicted_data, aes(x = num_spec, y = connect, color = region), size = 1) +  # Fitted inverse lines
  labs(
    x = "Number of Species",
    y = "Connectance",
    color = "Region",
    title = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top") 
#  geom_text(aes(label=ifelse(connect>0.15,as.character(paper),'')),hjust=0,vjust=0)

b

a + b


