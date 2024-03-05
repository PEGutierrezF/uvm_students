

library(readxl)
library(dplyr)
library(ggplot2)


data <- read_excel('Sum_Stream_Data.xlsx', sheet = 'Depth')
head(data,6)




# Summarize by stream and Pack
depth_new <- data %>%
  group_by(Stream) %>%
  summarise(Mean_depth = mean(Depth),
            SD_depth = sd(Depth))

depth_new

# Create a new column for grouping
depth_new$Group <- ifelse(depth_new$Stream %in% 
                                 c("Brown", "Stevensville"), "Forested", "Urban")                         


ggplot(depth_new, aes(x = Group, y = Mean_depth, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean_depth, ymax = Mean_depth + SD_depth),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "",
       x = "Stream",
       y = "Pool depth (cm)") +
  theme_classic() 


# ANOVA -------------------------------------------------------------------

mod1 <- aov(Depth~ Stream * landuse, data=data)
summary(mod1)
print(mod1)
