library(readxl)
library(dplyr)
library(ggplot2)


data<- read_xlsx('data.xlsx')
head(data,6)

# Create the plot
ggplot(data, aes(x = connect, y = num_spec, 
                 color = stream_order)) +
  geom_point(size = 3) +  
  labs(
    x = "Connectance",
    y = "Number of Species",
    color = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "top") 
