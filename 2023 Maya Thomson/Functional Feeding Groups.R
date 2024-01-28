



library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

data <- read_excel('2023 Maya Thomson/Sum_Stream_Data.xlsx', sheet = 'FFG')
head(data,6)

# Filtrar el dataframe para Pair1 (Brown y Stevenville)
Forested <- data[data$stream %in% c("Brown", "Stevenville"), ]


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


