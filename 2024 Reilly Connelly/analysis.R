library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

data<- read_xlsx('data.xlsx')
head(data,6)

mod1 <- lm(num_link~num_spec, data=data)
summary(mod1)
shapiro.test(residuals(mod1))


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
    color = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "top") 



# Create the plot with linear models for both regions on the same plot
a <- ggplot(data, aes(x = num_spec, y = num_link, color = region)) +
  geom_point(size = 3) +  # Points colored by region
  geom_smooth(method = "lm", se = F) +  # Add separate linear models for each region
  labs(
    x = "Number of Species",
    y = "num_link",
    color = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "top")  # Move the legend to the top
a


# Create the plot with linear models for both regions on the same plot
b <- ggplot(data, aes(x = connect, y = num_spec, color = region)) +
  geom_point(size = 3) +  # Points colored by region
  geom_smooth(method = "lm", se = F) +  # Add separate linear models for each region
  labs(
    x = "Connectance",
    y = "Number of Species",
    color = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "top")  # Move the legend to the top
b
a + b
