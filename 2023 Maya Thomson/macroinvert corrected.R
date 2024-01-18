

data <- read_excel('2023 Maya Thomson/Sum of Thesis Stream Data.xlsx', sheet = 'Macros')
head(data,6)


# Create a new column with the subtraction (Weight_initial - Weight_final) and keep Weight_final when it's higher
data$Subtraction_result <- ifelse(data$Weight_final > data$Weight_initial, 
                                  data$Weight_final, data$Weight_initial - data$Weight_final)


