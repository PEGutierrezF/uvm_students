



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

ancova_mod_links_log <- lm(log_num_link ~ log_num_spec * region, data = data)
summary(ancova_mod_links_log)
shapiro.test(residuals(ancova_mod_links_log))  # Check normality again


# Summary of the Slopes:
# Temperate Region:
  Slope = log_num_spec coefficient = 1.7022
# Tropical Region:
  Slope = 1.7022 + (-0.5367) = 1.1655


# Connectance -------------------------------------------------------------
mod2 <- lm(num_spec~connect, data=data)
summary(mod2)
shapiro.test(residuals(mod2))


# Create the plot# Create tdatahe plot
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


