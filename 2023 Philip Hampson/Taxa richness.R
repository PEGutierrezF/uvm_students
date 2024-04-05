



# ---------------------------------------------
# Taxa richness Mansfield
# 30 Mar 2023
# Pablo E. Guti?rrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


library(openxlsx)
library(dplyr)
library(ggplot2)
library(patchwork)

ranchbrook <- read.xlsx('data_2000_2024.xlsx', detectDates = TRUE)
head(ranchbrook,6)



# Merge columns and create a new column
ranchbrook$taxa_newname <- paste(ranchbrook$Order, ranchbrook$Family, 
                            ranchbrook$GenusGroup, ranchbrook$Genus, 
                            ranchbrook$SpeciesGroup, ranchbrook$Species, 
                            sep = '_')

# Unique number of taxa across the entire period
n_distinct(unique_taxa <- unique(ranchbrook$taxa_newname))

# Summarize per year
richness.ranchbrook <- ranchbrook %>%
  select(Date, taxa_newname)

richness <- richness.ranchbrook %>%
  group_by(Date) %>%
  summarise(taxa_richness = n_distinct(taxa_newname))



# Plot --------------------------------------------------------------------
richness$Date <- as.Date(richness$Date)

plot_richness <- ggplot(richness, aes(x = Date, y = taxa_richness)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(x = "Sampling Date", y = "Taxa richness") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  # ggtitle("Ranch Brook") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )
plot_richness



# lineal model ------------------------------------------------------------
shapiro.test(richness$taxa_richness)
mod <-  lm(taxa_richness ~ Date, data = richness)
summary(mod)
