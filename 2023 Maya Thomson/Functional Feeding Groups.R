



library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

data <- read_excel('Sum_Stream_Data.xlsx', sheet = 'FFG')
head(data,6)

# Filtrar el dataframe para Pair1 (Brown y Stevenville)
Forested <- data[data$stream %in% c("Brown", "Stevensville"), ]


# Create a 100% stacked bar chart
f <- ggplot(Forested, aes(x = stream, y = value, fill = ffg)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(value,1))),
            size = 5, position = position_stack(vjust = 0.5),
            colour = "black") +
  labs(title = "Forested streams",
       y = "Percentage",
       x = "")  +
  theme_classic() +
  theme(legend.position = "none")
f


# Filtrar el dataframe para Pair2 (Potash y Muddy)
urban <- data[data$stream %in% c("Potash", "Muddy"), ]

# Crear un 100% stacked bar chart con porcentajes en el eje y y etiquetas de texto
u <- ggplot(urban, aes(x = stream, y = value, fill = ffg)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(value,1))),
            size = 5, position = position_stack(vjust = 0.5),
            colour = "black") +
  labs(title = "Urban streams",
       y = "Percentage",
       x = "") +
  theme_classic() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

u

f + u



# Relative abundance ------------------------------------------------------
data <- read_excel('Sum_Stream_Data.xlsx', sheet = 'Macros')
head(data,6)


# Calculate the total count of each Functional_Group within each combination of Stream and Pack
data_summary <- data %>%
  group_by(Stream, Pack, Functional_Group) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate the total count of Functional_Group within each combination of Stream and Pack
data_summary <- data_summary %>%
  group_by(Stream, Pack) %>%
  mutate(total_count = sum(count)) %>%
  ungroup()

# Calculate the relative abundance of each Functional_Group within each combination of Stream and Pack
data_summary <- data_summary %>%
  mutate(relative_abundance = (count / total_count) * 100)

# Select only necessary columns
result <- data_summary %>% 
  select(Stream, Pack, Functional_Group, relative_abundance)

# Print the result
print(result)



# ANOVAs ------------------------------------------------------------------
# Add a column to categorize streams as "Forested" or "Urban" based on their names
data_summary <- data_summary %>%
  mutate(landuse = ifelse(Stream %in% c("Brown", "Stevensville"), "Forested", "Urban"))

# Filter for 'Shredder' Functional_Group
shredder_data <- data_summary %>%
  filter(Functional_Group == "Shredder")

shapiro.test(shredder_data$relative_abundance)

# Conduct ANOVA with 'relative_abundance' as the response and 'Stream' as the predictor
aov_shredder <- aov(relative_abundance ~ landuse + Stream, data = shredder_data)

# Print ANOVA results
summary(aov_shredder)



# Filter for 'Collector' Functional_Group
collector_data <- data_summary %>%
  filter(Functional_Group == "Collector")

shapiro.test(collector_data$relative_abundance)

# Conduct ANOVA with 'relative_abundance' as the response and 'Stream' as the predictor
aov_collector <- aov(relative_abundance ~landuse+Stream, data = collector_data)

# Print ANOVA results
summary(aov_collector)

# Perform Tukey post-hoc test
TukeyHSD(aov_collector)




# Filter for 'Scraper' Functional_Group
Scraper_data <- data_summary %>%
  filter(Functional_Group == "Scraper")

shapiro.test(Scraper_data$relative_abundance)
a <- log(Scraper_data$relative_abundance)
shapiro.test(a)

# Conduct ANOVA with 'relative_abundance' as the response and 'Stream' as the predictor
aov_scraper <- aov(a ~ landuse+Stream, data = Scraper_data)

# Print ANOVA results
summary(aov_scraper)



# Filter for 'Predator' Functional_Group
Predator_data <- data_summary %>%
  filter(Functional_Group == "Predator")

shapiro.test(Predator_data$relative_abundance)

# Conduct ANOVA with 'relative_abundance' as the response and 'Stream' as the predictor
aov_scraper <- aov(relative_abundance ~ landuse+Stream, data = Predator_data)

# Print ANOVA results
summary(aov_scraper)




# Plot --------------------------------------------------------------------
# Calculate the mean and standard deviation of the relative abundance for each Functional_Group within each Stream
mean_sd_relative <- data_summary %>%
  group_by(Stream, Functional_Group) %>%
  summarise(mean_relative_abundance = mean(relative_abundance),
            sd_relative_abundance = sd(relative_abundance))

# Print the result
print(mean_sd_relative)



# Reorder the Functional_Group variable as a factor with desired levels
mean_sd_relative$Functional_Group <- factor(mean_sd_relative$Functional_Group, levels = c("Shredder", "Collector","Filterers" ,"Scraper", "Predator"))

# Plotting
ggplot(mean_sd_relative, aes(x = Functional_Group, y = mean_relative_abundance, fill = Stream)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_relative_abundance - sd_relative_abundance, 
                    ymax = mean_relative_abundance + sd_relative_abundance),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = "Functional Group", y = "Mean Relative Abundance") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

