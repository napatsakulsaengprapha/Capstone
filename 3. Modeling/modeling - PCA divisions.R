library(dplyr)
library(rpart)
library(rpart.plot)
library(tree)
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

data_major <- data

data <- data[,-c(1,2)] #(pin, acad)

major_to_division <- function(Major) {
  n <- length(Major)
  x <- vector(mode = "character", length = n)
  for (i in 1:n){
    if (is.na(Major[i])) {x[i] <- NA}
    else if (Major[i] == "MCS") {x[i] <- "Sciences"}
    else if (Major[i] == "LS") {x[i] <- "Sciences"}
    else if (Major[i] == "PS") {x[i] <- "Sciences"}
    else if (Major[i] == "ES") {x[i] <- "Social Sciences"}
    else if (Major[i] == "PPE") {x[i] <- "Social Sciences"}
    else if (Major[i] == "PSY") {x[i] <- "Social Sciences"}
    else if (Major[i] == "ECON") {x[i] <- "Social Sciences"}
    else if (Major[i] == "GA") {x[i] <- "Social Sciences"}
    else if (Major[i] == "US") {x[i] <- "Social Sciences"}
    else if (Major[i] == "ANTH") {x[i] <- "Social Sciences"}
    else if (Major[i] == "HIST") {x[i] <- "Humanities"}
    else if (Major[i] == "LIT") {x[i] <- "Humanities"}
    else if (Major[i] == "ARTS HUM") {x[i] <- "Humanities"}
    else if (Major[i] == "PHIL") {x[i] <- "Humanities"}
  }
  return(x)
}

data$Division <- major_to_division(data$Major)
data$AI1 <- major_to_division(data$AI1)
data$AI2 <- major_to_division(data$AI2)

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI1, values_from = n, 
                     names_prefix = 'AI1_', values_fill = list(n = 0))

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI2, values_from = n, 
                     names_prefix = 'AI2_', values_fill = list(n = 0))


prin_comp <- prcomp(data[-c(1,10)], scale = TRUE)
summary(prin_comp)
plot(prin_comp, type = "l")

biplot(prin_comp, scale = 0)

str(prin_comp)
data2 <- cbind(data, prin_comp$x[,1:2])

ggplot(data2, aes(PC1, PC2, col= Division, fill = Division)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")



data2 <- cbind(data, prin_comp$x[,1:2])
#find certain groups
blue_top_cluster <- subset(data2, data2$Division == "Social Sciences" & data2$PC2 > 1)

ggplot(blue_top_cluster, aes(x = Major)) +
  geom_bar()

rest_of_blue <- subset(data2, data2$Division == "Social Sciences" & data2$PC2 <= 1)

ggplot(rest_of_blue, aes(x = Major)) +
  geom_bar()

#############################

green_bottom_cluster <- subset(data2, data2$Division == "Sciences" & data2$PC2 < -2)
ggplot(green_bottom_cluster, aes(x = Major)) +
  geom_bar()

rest_of_green <- subset(data2, data2$Division == "Sciences" & data2$PC2 >= -2)
ggplot(rest_of_green, aes(x = Major)) +
  geom_bar()

#topic discovery