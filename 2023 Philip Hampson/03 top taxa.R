



# Assuming your data frame is named 'df'
library(dplyr)

# Group by Date and Family, calculate the sum of BugSpeciesDensity for each group
grouped_data <- abund.ranchbrook %>%
  group_by(Date, Family) %>%
  summarise(total_density = sum(BugSpeciesDensity))

# Sort the data by Date and total_density in descending order
sorted_data <- grouped_data %>%
  arrange(Date, desc(total_density))

# Select the top 5 most abundant groups for each date
top_groups <- sorted_data %>%
  group_by(Date) %>%
  top_n(5)

# Output the result
print(top_groups)


# How many Top Taxa -------------------------------------------------------
# Extract unique taxa (Family) values from the top_groups data frame
unique_taxa <- unique(top_groups$Family)

# Output the unique taxa
print(unique_taxa)


# How many times (year) X taxa is the top ---------------------------------
# Count the occurrences of "CHIRONOMIDAE" in the Family column of top_groups
chironomidae_years <- sum(top_groups$Family == "CHIRONOMIDAE")
# Output the count
print(chironomidae_years)



# Years -------------------------------------------------------------------
# Filter top_groups for rows where Family is "CHIRONOMIDAE"
chironomidae_data <- top_groups %>%
  filter(Family == "CHIRONOMIDAE")

# Extract the year and abundance for Chironomidae
year_and_abundance <- chironomidae_data %>%
  select(Date, total_density)

# Output the result
print(year_and_abundance)




# Analysis per taxa -------------------------------------------------------
# Filter top_groups for Chironomidae
chironomidae_data <- top_groups %>%
  filter(Family == "CHIRONOMIDAE")

# Create a ggplot
p <- ggplot(chironomidae_data, aes(x = as.Date(Date), y = total_density)) +
  geom_point() +    # Scatter plot
  geom_smooth(method = "lm", se = T) +  # Fit a linear model
  labs(x = "Date", y = "Total Density", title = "Chironomidae Abundance Over Time") +
  theme_minimal()

# Print the plot
print(p)

# Fit a linear model
lm_model <- lm(total_density ~ as.Date(Date), data = chironomidae_data)

# Summary of the linear model
summary(lm_model)

shapiro.test(lm_model$residuals)
par(mfrow=c(2,2))
plot(lm_model)
par(mfrow=c(1,1))

hist(lm_model$residuals)
