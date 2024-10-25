library(readxl)
library(dplyr)
library(ggplot2)


data<- read_xlsx('data.xlsx')
head(data,6)

# Create the plot
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
b <- ggplot(data, aes(x = connect, y = num_spec, color = region)) +
  geom_point(size = 3) +  # Points colored by region
  geom_smooth(method = "lm", se = FALSE) +  # Add separate linear models for each region
  labs(
    x = "Connectance",
    y = "Number of Species",
    color = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "top")  # Move the legend to the top
b
a + b
