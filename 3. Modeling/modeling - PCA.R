library(factoextra)
library(ggplot2)
library(devtools)
library(cluster)
library(dplyr)

data<- read.csv("final_data.csv", stringsAsFactors = FALSE)

#make Major, AIs in to Dummies

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI1, values_from = n, 
                     names_prefix = 'AI1_', values_fill = list(n = 0))

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI2, values_from = n, 
                     names_prefix = 'AI2_', values_fill = list(n = 0))

data <- data[-c(1,2)] #remove PIN, Year (unsupervised)


prin_comp <- prcomp(data[-1], scale = TRUE)
summary(prin_comp)
plot(prin_comp, type = "l")

biplot(prin_comp, scale = 0)

str(prin_comp)
data2 <- cbind(data, prin_comp$x[,1:2])

ggplot(data2, aes(PC1, PC2, col= Major, fill = Major)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

ggplot(data2, aes(PC1, PC2)) +
  geom_point(shape = 21, col = "black")


