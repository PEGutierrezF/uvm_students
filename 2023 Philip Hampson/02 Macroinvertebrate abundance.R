



# ---------------------------------------------
# Abundance of macroinvertebrates
# 05 Apr 2024
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



library(openxlsx)
library(dplyr)
library(ggplot2)
library(patchwork)

ranchbrook <- read.xlsx('data_2000_2024.xlsx', detectDates = TRUE)
head(ranchbrook,6)


abund.ranchbrook <- ranchbrook %>%
  select(Date, BugSpeciesDensity, Family)
head(abund.ranchbrook, 10)


abundance <- abund.ranchbrook %>%
  group_by(Date) %>%
  summarise(mean = mean(BugSpeciesDensity), 
            sd = sd(BugSpeciesDensity))


# Plot --------------------------------------------------------------------
abundance$Date <- as.Date(abundance$Date)


plot <- ggplot(abundance, aes(x = Date, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = T) +
  labs(x = "Sampling Date", y = "Mean abundance") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  # ggtitle("Ranch Brook") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )
plot


# lineal model ------------------------------------------------------------
shapiro.test(abundance$mean)
abundance$mean_new <- log10(abundance$mean)
shapiro.test(abundance$mean_new)

mod3 <-  lm(mean_new ~ Date, data = abundance)
summary(mod3)


# Best transformation -----------------------------------------------------
AIC(mod2,mod3)

shapiro.test(mod2$residuals)
par(mfrow=c(2,2))
plot(mod2)
par(mfrow=c(1,1))

shapiro.test(mod3$residuals)
par(mfrow=c(2,2))
plot(mod3)
par(mfrow=c(1,1))
