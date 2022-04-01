install.packages("rpart.plot")
install.packages("tree")
library(dplyr)
library(rpart)
library(rpart.plot)
library(tree)
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

data <- data[-c(1,2)]

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

data$Major <- major_to_division(data$Major)
data$AI1 <- major_to_division(data$AI1)
data$AI2 <- major_to_division(data$AI2)

n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

sum(test_data$AI1 == test_data$Major)/nrow(test_data)


data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI1, values_from = n, 
                     names_prefix = 'AI1_', values_fill = list(n = 0))

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI2, values_from = n, 
                     names_prefix = 'AI2_', values_fill = list(n = 0))

n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set



Sciences.fit <- glm(Major == "Sciences" ~., data= train_data, family = binomial)

SocialSciences.fit <- glm(Major == "Social Sciences" ~., data= train_data, family = binomial)

Humanities.fit <- glm(Major == "Humanities" ~., data= train_data, family = binomial)


logistic_predictor <- function(test_data) {
  n <- nrow(test_data)
  predictor <- vector(mode = "character", length = n)
  for(i in 1:n) {
    prediction <- -10000
    major <- "empty"
    if (predict(Sciences.fit, test_data[i,]) > prediction) {
    prediction <- predict(Sciences.fit,test_data[i,]) ; major <- "Sciences" }
    if (predict(SocialSciences.fit, test_data[i,]) > prediction) {
    prediction <- predict(SocialSciences.fit,test_data[i,]) ; major <- "Social Sciences" }
    if (predict(Humanities.fit, test_data[i,]) > prediction) {
    prediction <- predict(Humanities.fit,test_data[i,]) ; major <- "Humanities" }
    predictor[i] <- major
  }
  return(predictor)
}


logistic_prediction <- logistic_predictor(test_data)
print(logistic_prediction)
unique(logistic_prediction) #only predicts 11 major, ARTS HUM, LIT, ECON
unique(test_data$Major)

check <- cbind(logistic_prediction, test_data$Major)
accuracy_Test <- sum(logistic_prediction == test_data$Major) / nrow(test_data)
print(paste('Accuracy for test', accuracy_Test)) #benchmark = 0.5956679



