



library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

data <- read_excel('2023 Maya Thomson/Sum_Stream_Data.xlsx', sheet = 'Macros')
head(data,6)


result <- data %>%
  group_by(Stream, Pack) %>%
  summarize(SumDistinctFamily = sum(n_distinct(Family)),
            FirstWeightFinal = first(Weight_final),
            total_corrected = SumDistinctFamily / FirstWeightFinal) %>%
  ungroup()


result



# Summarize by stream
summary_richness <- result %>%
  group_by(Stream) %>%
  summarise(Mean_Fam_richness = mean(total_corrected),
            SD_Fam_richness = sd(total_corrected))


# Create a new column for grouping
summary_richness$Group <- ifelse(summary_richness$Stream %in% 
                                   c("Brown", "Stevenspeil"), "Forested", "Urban")

# Create barplot
p_c <- ggplot(summary_richness, aes(x = Group, y = Mean_Fam_richness, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean_Fam_richness, ymax = Mean_Fam_richness + SD_Fam_richness),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "",
       x = "Stream",
       y = "Mean Richness") +
  theme_classic() +
  theme(legend.position = "none")
  

p_c



#ANOVA

anova_richness <- result %>%
  group_by(Stream, Pack) %>%
  slice(1) %>%
  ungroup() %>%
  select(Stream, total_corrected)

shapiro.test(anova_richness$total_corrected)
new_total_corrected <- 1/sqrt((anova_richness$total_corrected))
shapiro.test(new_total_corrected)


# Perform one-way ANOVA
result_anova <- aov(new_total_corrected ~ Stream, data = result)

# Display the ANOVA table
summary(result_anova)




# Abundance ---------------------------------------------------------------


# Assuming your_data is your dataset
result_abudance <- data %>%
  group_by(Stream, Pack) %>%
  reframe(Abundance = n(),
    Subtraction_Result = first(Weight_final),
    Abundance_Divided = Abundance / Weight_final) %>%
  print()


# Calculate mean and standard deviation for each stream
summary_abundance <- result_abudance %>%
  group_by(Stream) %>%
  summarize(mean_abundance = mean(Abundance_Divided),
            sd_abundance = sd(Abundance_Divided))


# Create a new column for grouping
summary_abundance$Group <- ifelse(summary_abundance$Stream %in% 
                                    c("Brown", "Stevenspeil"), "Forested", "Urban")

# Create barplot for abundance
q_c <- ggplot(summary_abundance, aes(x = Group, y = mean_abundance, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_abundance, ymax = mean_abundance + sd_abundance),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "",
       x = "Stream",
       y = "Mean Abundance") +
  theme_classic()

q_c



p_c + q_c
