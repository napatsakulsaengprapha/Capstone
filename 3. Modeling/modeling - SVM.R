install.packages('e1071')
library(e1071)
library(factoextra)
library(ggplot2)
library(devtools)
library(cluster)
library(dplyr)

#https://www.geeksforgeeks.org/classifying-data-using-support-vector-machinessvms-in-r/

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

data$Major <- as.factor(data$Major)

data <- data[-c(1,2)] #remove PIN, Year
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

train_data[-1] <- scale(train_data[-1])
test_data[-1] <- scale(test_data[-1])


#linear
classifier = svm(formula = Major ~ .,
                 data = train_data,
                 type = 'C-classification',
                 kernel = 'linear')

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm)



#radial
classifier = svm(formula = Major ~ .,
                 data = train_data,
                 scale = FALSE, 
                 kernel = "radial", 
                 cost = 1)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm)


#sigmoid - HIGHEST
classifier = svm(formula = Major ~ .,
                 data = train_data,
                 scale = FALSE, 
                 kernel = "sigmoid", 
                 cost = 1)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm)


#polynomial
classifier = svm(formula = Major ~ .,
                 data = train_data,
                 scale = FALSE, 
                 kernel = "polynomial", 
                 cost = 1)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm)


#optimize sigmoid

#sigmoid - HIGHEST
classifier = svm(formula = Major ~ .,
                 data = train_data,
                 scale = FALSE, 
                 kernel = "sigmoid", 
                 cost = .35,
                 coef0 = 0,
                 shrinking = TRUE)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm)


####################################################### just grades
classifier = svm(formula = Major ~ LH1 + LH2 + PPT1 + PPT2 + CSI + MST + QR + Science,
                 data = train_data,
                 scale = FALSE, 
                 kernel = "sigmoid", 
                 cost = .35,
                 coef0 = 0,
                 shrinking = TRUE)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm)

####################################################### just AI
train_data <- train_data[-c(2:9)]
test_data <- test_data[-c(2:9)]

classifier = svm(formula = Major ~.,
                 data = train_data,
                 scale = FALSE, 
                 kernel = "sigmoid", 
                 cost = .35,
                 coef0 = 0,
                 shrinking = TRUE)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm)


####################################################### unbalanced fix
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

data$Major <- as.factor(data$Major)

data <- data[-c(1,2)] #remove PIN, Year
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

train_data[-1] <- scale(train_data[-1])
test_data[-1] <- scale(test_data[-1])


train_data <- train_data[-c(2:9)]
test_data <- test_data[-c(2:9)]

train_data <- downSample(train_data[,-3], train_data$Major, list = FALSE, yname = "Major")

classifier = svm(formula = Major ~.,
                 data = train_data,
                 scale = FALSE, 
                 kernel = "sigmoid", 
                 cost = .35,
                 coef0 = 0,
                 shrinking = TRUE)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm) ##29


train_data <- upSample(train_data[,-3], train_data$Major, list = FALSE, yname = "Major")

classifier = svm(formula = Major ~.,
                 data = train_data,
                 scale = FALSE, 
                 kernel = "sigmoid", 
                 cost = .35,
                 coef0 = 0,
                 shrinking = TRUE)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm) ##29


####################################################################################### division
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)
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
#make Major, AIs in to Dummies

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI1, values_from = n, 
                     names_prefix = 'AI1_', values_fill = list(n = 0))

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI2, values_from = n, 
                     names_prefix = 'AI2_', values_fill = list(n = 0))

data$Major <- as.factor(data$Major)

data <- data[-c(1,2)] #remove PIN, Year
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

train_data[-1] <- scale(train_data[-1])
test_data[-1] <- scale(test_data[-1])

classifier = svm(formula = Major ~ .,
                 data = train_data,
                 scale = FALSE, 
                 kernel = "sigmoid", 
                 cost = 1)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm)


##############################################################fix unbalanced
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

data$Major <- as.factor(data$Major)

data <- data[-c(1,2)] #remove PIN, Year
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

train_data[-1] <- scale(train_data[-1])
test_data[-1] <- scale(test_data[-1])

library(rpart)
library(ROSE)
library(rpart.plot)
library(tree)
require(tree)
library(caret)


sum(test_data$AI1 == test_data$Major)/nrow(test_data) #0.3465704
is.factor(data$Major)
data_balanced_down <- downSample(train_data[,-3], train_data$Major, list = FALSE, yname = "Major")

is.factor(data$Major)
data_balanced_up <- upSample(train_data[,-3], train_data$Major, list = FALSE, yname = "Major") #26%

classifier = svm(formula = Major ~ ., #28%
                 data = data_balanced_down,
                 scale = FALSE, 
                 kernel = "sigmoid", 
                 cost = .35,
                 coef0 = 0,
                 shrinking = TRUE)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm)


classifier = svm(formula = Major ~ ., #31%
                 data = data_balanced_up,
                 scale = FALSE, 
                 kernel = "sigmoid", 
                 cost = .35,
                 coef0 = 0,
                 shrinking = TRUE)

major_pred = predict(classifier, newdata = test_data[-1])

cm = table(test_data$Major, major_pred)
cm

sum(diag(cm))/sum(cm)

