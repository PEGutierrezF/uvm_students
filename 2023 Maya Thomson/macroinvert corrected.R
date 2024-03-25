



library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

data <- read_excel('Sum_Stream_Data.xlsx', sheet = 'Macros')
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
                                   c("Brown", "Stevensville"), "Forested", "Urban")

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

anova_richness <- anova_richness %>%
  mutate(landuse = ifelse(Stream %in% c("Brown", "Stevensville"), "Forested", "Urban"))

shapiro.test(anova_richness$total_corrected)
new_total_corrected <- 1/sqrt((anova_richness$total_corrected))
shapiro.test(new_total_corrected)


# Perform one-way ANOVA
result_anova <- aov(new_total_corrected ~ landuse*Stream, data = anova_richness)

# Display the ANOVA table
summary(result_anova)




# Abundance ---------------------------------------------------------------

# Count and sum characters in the Family column by Stream and Pack
result_abundance <- data %>%
  group_by(Stream, Pack) %>%
  summarize(abundance = n(),
            FirstWeightFinal = first(Weight_final),
            abunndance_corrected = abundance / FirstWeightFinal)

result_abundance


# Calculate mean and standard deviation for each stream
summary_abundance <- result_abundance %>%
  group_by(Stream) %>%
  summarize(mean_abundance = mean(abunndance_corrected),
            sd_abundance = sd(abunndance_corrected))


# Create a new column for grouping
summary_abundance$Group <- ifelse(summary_abundance$Stream %in% 
                                    c("Brown", "Stevensville"), "Forested", "Urban")

# Create barplot for abundance
q_c <- ggplot(summary_abundance, aes(x = Group, y = mean_abundance, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_abundance, ymax = mean_abundance + sd_abundance),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "",
       x = "Stream",
       y = "Mean Abundance") +
  theme_classic()+
  theme(legend.position = "none")

q_c



p_c + q_c




#ANOVA

anova_abundance <- result_abundance %>%
  group_by(Stream, Pack) %>%
  slice(1) %>%
  ungroup() %>%
  select(Stream, abunndance_corrected)

anova_abundance <- anova_abundance %>%
  mutate(landuse = ifelse(Stream %in% c("Brown", "Stevensville"), "Forested", "Urban"))

shapiro.test(anova_abundance$abunndance_corrected)
new_abunndance_corrected<-log(anova_abundance$abunndance_corrected)
shapiro.test(new_abunndance_corrected)


# Perform one-way ANOVA
result_anova <- aov(new_abunndance_corrected ~ landuse * Stream, data = anova_abundance)

# Display the ANOVA table
summary(result_anova)

# Perform Tukey post-hoc test
TukeyHSD(result_anova)




# Biomass -----------------------------------------------------------------
# Sum biomass column by Stream and Pack
result_biomass <- data %>%
  group_by(Stream, Pack) %>%
  summarize(biomass = sum(biomass),
            FirstWeightFinal = first(Weight_final),
            biomass_corrected = biomass / FirstWeightFinal)

result_biomass


# Calculate mean and standard deviation for each stream
summary_biomass <- result_biomass %>%
  group_by(Stream) %>%
  summarize(mean_biomass = mean(biomass_corrected),
            sd_biomass = sd(biomass_corrected))


# Create a new column for grouping
summary_biomass$Group <- ifelse(summary_biomass$Stream %in% 
                                    c("Brown", "Stevensville"), "Forested", "Urban")



# Create barplot for abundance
biomass_c <- ggplot(summary_biomass, aes(x = Group, y = mean_biomass, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_biomass, ymax = mean_biomass + sd_biomass),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "",
       x = "Stream",
       y = "Mean Biomass (mg AFDM/g)") +
  theme_classic()

p_c + q_c + biomass_c



anova_biomass <- result_biomass %>%
  group_by(Stream, Pack) %>%
  slice(1) %>%
  ungroup() %>%
  select(Stream, biomass_corrected)

anova_biomass <- anova_biomass %>%
  mutate(landuse = ifelse(Stream %in% c("Brown", "Stevensville"), "Forested", "Urban"))

shapiro.test(anova_biomass$biomass_corrected)
new_biomass_corrected <- 1/sqrt((anova_biomass$biomass_corrected))
shapiro.test(new_biomass_corrected)


# Perform one-way ANOVA
result_biomass_anova <- aov(new_biomass_corrected ~ Stream + landuse, data = anova_biomass)

# Display the ANOVA table
summary(result_biomass_anova)

# Perform Tukey post-hoc test
TukeyHSD(result_biomass_anova)

