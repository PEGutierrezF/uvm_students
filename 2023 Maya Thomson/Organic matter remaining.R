



library(readxl)
library(dplyr)
library(ggplot2)


data <- read_excel('2023 Maya Thomson/Sum_Stream_Data.xlsx', sheet = 'Macros')
head(data,6)


# Summarize by stream
data_remaining <- data %>%
  group_by(Stream) %>%
  summarise(Mean_remaining = mean(Weight_final),
            SD_remaining = sd(Weight_final))

# Create a new column for grouping
data_remaining$Group <- ifelse(data_remaining$Stream %in% 
                                   c("Brown", "Stevenspeil"), "Forested", "Urban")                         
                                   
                                   
ggplot(data_remaining, aes(x = Group, y = Mean_remaining, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean_remaining, ymax = Mean_remaining + SD_remaining),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "",
       x = "Stream",
       y = "Mean organic matter remaining (g)") +
  theme_classic()



###########################################################################
# ANOVA -------------------------------------------------------------------
###########################################################################

data_anova <- data %>%
  group_by(Stream, Pack, Replicate) %>%
  slice(1) %>%
  ungroup() %>%
  select(Stream, Weight_final)

# Print the new table
print(data_anova)

shapiro.test(data_anova$Weight_final)
new_Weight_final <- log(data_anova$Weight_final+1)
shapiro.test(new_Weight_final)


# Perform one-way ANOVA
result_anova <- aov(new_Weight_final ~ Stream, data = new_table)

# Display the ANOVA table
summary(result_anova)

# Perform Tukey post-hoc test
tukey_result <- TukeyHSD(result_anova)

# Display the Tukey post-hoc test results
print(tukey_result)
plot(tukey_result)
