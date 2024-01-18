

data <- read_excel('2023 Maya Thomson/Sum of Thesis Stream Data.xlsx', sheet = 'Macros')
head(data,6)


# Create a new column with the subtraction (Weight_initial - Weight_final) and keep Weight_final when it's higher
data$Subtraction_result <- ifelse(data$Weight_final > data$Weight_initial, 
                                  data$Weight_final, data$Weight_initial - data$Weight_final)



# Assuming your_data is your dataset
result <- data %>%
  group_by(Stream, Pack) %>%
  summarize(
    Mean = n_distinct(Family),
    Subtraction_Result = first(Subtraction_result),
    Mean_Divided = Mean / Subtraction_Result) %>%
  print()



# Summarize by stream
summary_richness <- result %>%
  group_by(Stream) %>%
  summarise(Mean_Fam_richness = mean(Mean_Divided),
            SD_Fam_richness = sd(Mean_Divided))


# Create a new column for grouping
summary_richness$Group <- ifelse(summary_richness$Stream %in% 
                                   c("Brown", "Stevenspeil"), "Forested", "Urban")

# Create barplot
p <- ggplot(summary_richness, aes(x = Group, y = Mean_Fam_richness, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean_Fam_richness, ymax = Mean_Fam_richness + SD_Fam_richness),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "Mean Ricness and SD Barplot",
       x = "Stream",
       y = "Mean Richness") +
  theme_minimal()
p





# Assuming your_data is your dataset
result_abudance <- data %>%
  group_by(Stream, Pack) %>%
  summarize(
    Abundance = n(),
    Subtraction_Result = first(Subtraction_result),
    Abundance_Divided = Abundance / Subtraction_Result
  ) %>%
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
q <- ggplot(summary_abundance, aes(x = Group, y = mean_abundance, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_abundance, ymax = mean_abundance + sd_abundance),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "Mean Abundance and SD Barplot",
       x = "Stream",
       y = "Mean Abundance") +
  theme_minimal()

q
