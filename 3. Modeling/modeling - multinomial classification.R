library(dplyr)
library(rpart)
library(rpart.plot)
library(tree)
library(caret)
library(readr)
library(nnet)
#https://www.r-bloggers.com/2020/05/multinomial-logistic-regression-with-r/


data <- read.csv("final_data.csv", stringsAsFactors = FALSE)
data$Major <- as.factor(data$Major)
data$AI1 <- as.factor(data$AI1)
data$AI2 <- as.factor(data$AI2)

data <- data[-c(1,2,4)] #remove AI2 because of NAs
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set


multinom_model <- multinom(Major ~ ., data = train_data)
summary(multinom_model)
exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))

test_data$ClassPredicted <- predict(multinom_model, newdata = test_data, "class")
tab <- table(test_data$Major, test_data$ClassPredicted)
tab

sum(diag(tab))/sum(tab) #results exactly equal benchmark



multinom_model <- multinom(Major ~ AI1, data = train_data)
summary(multinom_model)
exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))

test_data$ClassPredicted <- predict(multinom_model, newdata = test_data, "class")
tab <- table(test_data$Major, test_data$ClassPredicted)
tab

sum(diag(tab))/sum(tab)


########################################################################### just grades

data <- read.csv("final_data.csv", stringsAsFactors = FALSE)
data$Major <- as.factor(data$Major)
data$AI1 <- as.factor(data$AI1)
data$AI2 <- as.factor(data$AI2)

data <- data[-c(1,2,4)] #remove AI2 because of NAs
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set


multinom_model <- multinom(Major ~ LH1 + LH2 + PPT1 + PPT2 + CSI + MST + QR + Science, data = train_data)
summary(multinom_model)
exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))

test_data$ClassPredicted <- predict(multinom_model, newdata = test_data, "class")
tab <- table(test_data$Major, test_data$ClassPredicted)
tab

sum(diag(tab))/sum(tab) #results exactly equal benchmark

########################################################################### division

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
    else if (Major[i] == "ES") {x[i] <- "Sciences"}
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

data$Major <- as.factor(data$Major)
data$AI1 <- as.factor(data$AI1)
data$AI2 <- as.factor(data$AI2)

data <- data[-c(1,2,4)] #remove AI2 because of NAs
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set


multinom_model <- multinom(Major ~ ., data = train_data)
summary(multinom_model)
exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))

test_data$ClassPredicted <- predict(multinom_model, newdata = test_data, "class")
tab <- table(test_data$Major, test_data$ClassPredicted)
tab

sum(diag(tab))/sum(tab) #results exactly equal benchmark

########################################################## unbalanced

data <- read.csv("final_data.csv", stringsAsFactors = FALSE)
data$Major <- as.factor(data$Major)
data$AI1 <- as.factor(data$AI1)
data$AI2 <- as.factor(data$AI2)

data <- data[-c(1,2,4)] #remove AI2 because of NAs
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

sum(test_data$AI1 == test_data$Major)/nrow(test_data) 
is.factor(data$Major)
data_balanced_down <- downSample(train_data[,-3], train_data$Major, list = FALSE, yname = "Major")

is.factor(data$Major)
data_balanced_up <- upSample(train_data[,-3], train_data$Major, list = FALSE, yname = "Major")



multinom_model <- multinom(Major ~ ., data = data_balanced_up)
summary(multinom_model)
exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))

test_data$ClassPredicted <- predict(multinom_model, newdata = test_data, "class")
tab <- table(test_data$Major, test_data$ClassPredicted)
tab

sum(diag(tab))/sum(tab) #28.8


multinom_model <- multinom(Major ~ ., data = data_balanced_down)
summary(multinom_model)
exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))

test_data$ClassPredicted <- predict(multinom_model, newdata = test_data, "class")
tab <- table(test_data$Major, test_data$ClassPredicted)
tab

sum(diag(tab))/sum(tab) #26.7

