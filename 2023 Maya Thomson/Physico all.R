


library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(vegan)
install.packages("ade4")
library(ade4)

data <- read_excel('Sum_Stream_Data.xlsx', sheet = 'Phys')
head(data,6)


# Calculate mean and standard deviation per stream and variable
summary_data <- data %>%
  group_by(stream, variable) %>%
  summarize(Mean = mean(value), SD = sd(value))


# Define the order of streams and variables
stream_order <- c("Stevensville", "Brown", "Muddy", "Potash")
variable_order <- c("Temperature", "Conductivity", "pH")

# Convert 'stream' and 'variable' to factors with the specified order
summary_data$stream <- factor(summary_data$stream, levels = stream_order)
summary_data$variable <- factor(summary_data$variable, levels = variable_order)

# Plot using ggplot2 with facets and individual scales without legend
ggplot(summary_data, aes(x = stream, y = Mean)) +
  geom_point(aes(color = variable), position = position_dodge(width = 0.5), size = 5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD, color = variable), position = position_dodge(width = 0.5), width = 0.2) +
  facet_wrap(~ variable, scales = "free_y", strip.position = "top", drop = TRUE) +
  labs(title = "",
       x = "Stream",
       y = "Mean") +
  theme_bw() +
  theme(legend.position = "none")



# Subset the dataset for Temperature variable
data_temperature <- subset(data, variable == "Temperature")

# Two-way PERMANOVA
result_temperature <- adonis2(data_temperature$value ~ landuse + stream, data_temperature)
print(result_temperature)



install.packages('devtools')
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis") 
library(pairwiseAdonis)

pair.mod <- pairwise.adonis(data_temperature, factors=data_temperature$stream)
pair.mod



