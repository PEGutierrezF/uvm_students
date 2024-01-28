



library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

data <- read_excel('2023 Maya Thomson/Sum_Stream_Data.xlsx', sheet = 'FFG')
head(data,6)

# Set the desired order for the stream variable
data$stream <- factor(data$stream, levels = c("Brown", "Stevenville", "Potash", "Muddy"))

# Create a 100% stacked bar chart
ggplot(data, aes(x = stream, y = value, fill = ffg)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       y = "Percentage",
       x = "") 


