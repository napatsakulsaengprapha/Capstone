#install.packages("factoextra")
#install.packages("devtools")
library(factoextra)
library(ggplot2)
library(devtools)
library(cluster)
library(dplyr)

data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

#make Major, AIs in to Dummies

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI1, values_from = n, 
                     names_prefix = 'AI1_', values_fill = list(n = 0))

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI2, values_from = n, 
                     names_prefix = 'AI2_', values_fill = list(n = 0))

data_division <- data[-c(1,2)] #for comparison
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

data_division$Major <- major_to_division(data$Major)

data_major <- data[-c(1,2)] #for comparison

data <- data[-c(1,2,3)] #remove PIN, Year, Major (unsupervised)
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(data, 15, 1234)

set.seed(123)
kmean <- kmeans(data, 3, nstart = 25, iter.max = 100)
kmean$centers

fviz_cluster(kmean, data = data,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

data_division$kmean_group <- kmean$cluster
natural_group_division <- table(data_division$Major, data_division$kmean_group)
natural_group_division #some sort of standardization to account for more number of social sciences?

sum(data_division$Major == "Sciences")
sum(data_division$Major == "Social Sciences")
sum(data_division$Major == "Humanities")

scaled_natural_group_division <- rbind((natural_group_division[1,]/sum(data_division$Major == "Humanities")), 
                                       (natural_group_division[2,]/sum(data_division$Major == "Sciences")), 
                                        (natural_group_division[3,]/sum(data_division$Major == "Social Sciences")))
scaled_natural_group_division

set.seed(123)
kmean <- kmeans(data, 14, nstart = 25, iter.max = 100)
kmean$centers

fviz_cluster(kmean, data = data,
             palette = c("#ef1101","#5c0300","#fd9206","#a45d00","#fef636","#8f8a00","#0efd0e","#009a00","#010afb","#0006a4","#ba20fd","#7600a8","#080f0f","#faffff"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

kmean$cluster
data_major$kmean_group <- kmean$cluster
natural_group_division <- table(data_major$Major, data_major$kmean_group)
natural_group_division


set.seed(123)
kmean <- kmeans(data, 4, nstart = 25, iter.max = 100)
kmean$centers

fviz_cluster(kmean, data = data,
             palette = c("#ef1101","#5c0300","#fd9206","#a45d00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


#adding labels to K-means

#########################################################################################################

data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

#make Major, AIs in to Dummies

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI1, values_from = n, 
                     names_prefix = 'AI_', values_fill = list(n = 0))

either_or <- function(data){
  n <- nrow(data)
  for (i in 1:n){
    if (is.na(data$AI2[i])) {}
    else if (data$AI2[i] == "PSY") {data$AI_PSY[i] <- 1}
    else if (data$AI2[i] == "ECON") {data$AI_ECON[i] <- 1}
    else if (data$AI2[i] == "ANTH") {data$AI_ANTH[i] <- 1}
    else if (data$AI2[i] == "ARTS HUM") {data$`AI_ARTS HUM`[i] <- 1}
    else if (data$AI2[i] == "LIT") {data$AI_LIT[i] <- 1}
    else if (data$AI2[i] == "GA") {data$AI_GA[i] <- 1}
    else if (data$AI2[i] == "PHIL") {data$AI_PHIL[i] <- 1}
    else if (data$AI2[i] == "HIST") {data$AI_HIST[i] <- 1}
    else if (data$AI2[i] == "PS") {data$AI_PS[i] <- 1}
    else if (data$AI2[i] == "LS") {data$AI_LS[i] <- 1}
    else if (data$AI2[i] == "MCS") {data$AI_MCS[i] <- 1}
    else if (data$AI2[i] == "ES") {data$AI_ES[i] <- 1}
    else if (data$AI2[i] == "US") {data$AI_US[i] <- 1}
    else if (data$AI2[i] == "PPE") {data$AI_PPE[i] <- 1}
  }
  return(data)
}


data <- either_or(data)

either_or(data)

data_division <- data[-c(1,2)] #for comparison
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

data_division$Major <- major_to_division(data$Major)

data_major <- data[-c(1,2)] #for comparison

data <- data[-c(1,2,3,4)] #remove PIN, Year, Major (unsupervised)
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(data, 15, 1234)

set.seed(123)
kmean <- kmeans(data, 3, nstart = 25, iter.max = 100)
kmean$centers

fviz_cluster(kmean, data = data,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

data_division$kmean_group <- kmean$cluster
natural_group_division <- table(data_division$Major, data_division$kmean_group)
natural_group_division #some sort of standardization to account for more number of social sciences?

sum(data_division$Major == "Sciences")
sum(data_division$Major == "Social Sciences")
sum(data_division$Major == "Humanities")

scaled_natural_group_division <- rbind((natural_group_division[1,]/sum(data_division$Major == "Humanities")), 
                                       (natural_group_division[2,]/sum(data_division$Major == "Sciences")), 
                                       (natural_group_division[3,]/sum(data_division$Major == "Social Sciences")))
scaled_natural_group_division

set.seed(123)
kmean <- kmeans(data, 14, nstart = 25, iter.max = 100)
kmean$centers

fviz_cluster(kmean, data = data,
             palette = c("#ef1101","#5c0300","#fd9206","#a45d00","#fef636","#8f8a00","#0efd0e","#009a00","#010afb","#0006a4","#ba20fd","#7600a8","#080f0f","#faffff"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

kmean$cluster
data_major$kmean_group <- kmean$cluster
natural_group_division <- table(data_major$Major, data_major$kmean_group)
natural_group_division


set.seed(123)
kmean <- kmeans(data, 5, nstart = 25, iter.max = 100)
kmean$centers

fviz_cluster(kmean, data = data,
             palette = c("#ef1101","#5c0300","#fd9206","#a45d00","#fef636"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



