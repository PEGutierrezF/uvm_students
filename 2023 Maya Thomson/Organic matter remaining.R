



library(readxl)
library(dplyr)
library(ggplot2)


data <- read_excel('2023 Maya Thomson/Sum of Thesis Stream Data.xlsx', sheet = 'Macros')
head(data,6)


# Create a new column with the subtraction (Weight_initial - Weight_final) and keep Weight_final when it's higher
data$Subtraction_result <- ifelse(data$Weight_final > data$Weight_initial, 
                                  data$Weight_final, data$Weight_initial - data$Weight_final)



# Summarize by stream
data_remaining <- data %>%
  group_by(Stream) %>%
  summarise(Mean_remaining = mean(Subtraction_result),
            SD_remaining = sd(Subtraction_result))

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
  theme_classic() +
  theme(legend.position = "none")

