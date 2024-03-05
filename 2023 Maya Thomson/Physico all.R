


library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(vegan)
library(ade4)
require(pairwiseAdonis)

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




# Temperature -------------------------------------------------------------
# Subset the dataset for Temperature variable
data_temperature <- subset(data, variable == "Temperature")

# Two-way PERMANOVA
result_temperature <- adonis2(data_temperature$value ~ landuse + stream, data_temperature)
print(result_temperature)


dist.temp <- vegdist(data_temperature$value, method="euclidean")
pairwise.adonis2(dist.temp ~ landuse, 
                 data = data_temperature, permutations = 999)


dist.temp.stream <- vegdist(data_temperature$value, method="euclidean")
pairwise.adonis2(dist.temp.stream ~ stream, 
                 data = data_temperature, permutations = 999)



# pH ----------------------------------------------------------------------
# Subset the dataset for pH variable
data_pH <- subset(data, variable == "pH")

# Two-way PERMANOVA
result_pH <- adonis2(data_pH$value ~ landuse + stream, data_pH)
print(result_pH)


dist.pH <- vegdist(data_pH$value, method="euclidean")
pairwise.adonis2(dist.pH ~ landuse, 
                 data = data_pH, permutations = 999)


dist.pH.stream <- vegdist(data_pH$value, method="euclidean")
pairwise.adonis2(dist.pH.stream ~ stream, 
                 data = data_pH, permutations = 999)



# conductivity----------------------------------------------------------------------
# Subset the dataset for pH variable
data_cond<- subset(data, variable == "Conductivity")

# Two-way PERMANOVA
result_cond <- adonis2(data_cond$value ~ landuse + stream, data_cond)
print(result_cond)


dist.cond <- vegdist(data_cond$value, method="euclidean")
pairwise.adonis2(dist.cond.stream ~ landuse, 
                 data = data_cond, permutations = 999)

dist.cond.stream <- vegdist(data_cond$value, method="euclidean")
pairwise.adonis2(dist.cond.stream ~ stream, 
                 data = data_cond, permutations = 999)
