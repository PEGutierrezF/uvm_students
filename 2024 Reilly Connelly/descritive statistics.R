



# --------------------------------------------------------
# Descriptive statistics
# Date: Fri Feb 21 2025 17:11:22
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------

# Read data from Excel
data <- read_xlsx('data.xlsx', sheet = 'final_data')

# Split data for Temperate region
temperate_data <- data %>%
  filter(region == "Temperate")  # Adjust based on actual region names

# Get top 5 lowest and highest connectance values by region
lowest_connectance <- data %>%
  group_by(region) %>%
  arrange(connect) %>%
  slice_head(n = 5)  # Get 5 lowest values

highest_connectance <- data %>%
  group_by(region) %>%
  arrange(desc(connect)) %>%
  slice_head(n = 5)  # Get 5 highest values

# Print results
print("Top 5 Lowest Connectance by Region:")
print(lowest_connectance)

print("Top 5 Highest Connectance by Region:")
print(highest_connectance)
