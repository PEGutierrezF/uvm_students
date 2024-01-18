


library(readxl)
library(dplyr)
library(ggplot2)

data <- read_excel('2023 Maya Thomson/Sum of Thesis Stream Data.xlsx', sheet = 'Macros')
head(data,6)


# Sum total of individuals by Stream, Pack, Family
richness <- data %>%
  group_by(Stream, Pack) %>%
  summarize(Family_richness = n_distinct(Family))

head(richness, 20)

# Summarize by stream
summary_richness <- richness %>%
  group_by(Stream) %>%
  summarise(Mean_Fam_richness = mean(Family_richness),
    SD_Fam_richness = sd(Family_richness))


# Create a new column for grouping
summary_richness$Group <- ifelse(summary_richness$Stream %in% 
                                   c("Brown", "Stevenspeil"), "Forested", "Urban")

# Create barplot
p <- ggplot(summary_richness, aes(x = Group, y = Mean_Fam_richness, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean_Fam_richness - SD_Fam_richness, ymax = Mean_Fam_richness + SD_Fam_richness),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "Mean Abundance and SD Barplot",
       x = "Stream",
       y = "Mean Richness") +
  theme_minimal()
p



# Count and sum characters in the Family column by Stream and Pack
abundance <- data %>%
  group_by(Stream, Pack) %>%
  summarize(abundance = n()) 
  

abundance


# General -----------------------------------------------------------------
# Calculate mean and standard deviation for each stream
summary_abundance <- abundance %>%
  group_by(Stream) %>%
  summarize(mean_abundance = mean(abundance),
            sd_abundance = sd(abundance))


# Create a new column for grouping
summary_abundance$Group <- ifelse(summary_abundance$Stream %in% 
                                    c("Brown", "Stevenspeil"), "Forested", "Urban")

# Create barplot for abundance
q <- ggplot(summary_abundance, aes(x = Group, y = mean_abundance, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_abundance - sd_abundance, ymax = mean_abundance + sd_abundance),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "Mean Abundance and SD Barplot",
       x = "Stream",
       y = "Mean Abundance") +
  theme_minimal()

q

library(patchwork)
p + q



