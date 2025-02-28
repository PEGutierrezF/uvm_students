



# --------------------------------------------------------
# Descriptive statistics
# Date: Fri Feb 21 2025 17:11:22
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------

# Read data from Excel
data <- read_xlsx('2024 Reilly Connelly/data.xlsx', sheet = 'final_data')
# Exclude the last row
data <- data[-nrow(data), ]# (Yule et al. 2010)

# Split data for Temperate region
temperate_data <- data %>%
  filter(region == "Temperate")  # Adjust based on actual region names

# Get top 5 lowest and highest connectance values by region
lowest_connectance <- data %>%
  group_by(region) %>%
  arrange(connect) %>%
  slice_head(n = 1)  # Get 1 lowest values

highest_connectance <- data %>%
  group_by(region) %>%
  arrange(desc(connect)) %>%
  slice_head(n = 1)  # Get 1 highest values

# Print results
print("Top 5 Lowest Connectance by Region:")
print(lowest_connectance)

print("Top 5 Highest Connectance by Region:")
print(highest_connectance)


# Species richness ----------------------------------------------------------
# Get top 5 lowest and highest Species richness values by region
lowest_num_spec <- data %>%
  group_by(region) %>%
  arrange(num_spec) %>%
  slice_head(n = 1)  # Get 1 lowest values
lowest_num_spec

highest_num_spec <- data %>%
  group_by(region) %>%
  arrange(desc(num_spec)) %>%
  slice_head(n = 1)  # Get 1 highest values
highest_num_spec


# Number of links ----------------------------------------------------------
# Get top 5 lowest and highest Number of links values by region
lowest_num_link <- data %>%
  group_by(region) %>%
  arrange(num_link ) %>%
  slice_head(n = 1)  # Get 1 lowest values
lowest_num_link 

highest_num_link <- data %>%
  group_by(region) %>%
  arrange(desc(num_link )) %>%
  slice_head(n = 1)  # Get 1 highest values
highest_num_link
