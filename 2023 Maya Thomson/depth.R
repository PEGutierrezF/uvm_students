

library(readxl)
library(dplyr)
library(ggplot2)


data <- read_excel('2023 Maya Thomson/Sum of Thesis Stream Data.xlsx', sheet = 'Depth')
head(data,6)




# Summarize by stream and Pack
depth <- data %>%
  group_by(Stream,Pack) %>%
  summarise(Mean_depth = mean(Depth))


# Summarize by stream and Pack
depth_new <- depth %>%
  group_by(Stream) %>%
  summarise(Mean_depth = mean(Mean_depth),
            SD_depth = sd(Mean_depth))

depth_new

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
