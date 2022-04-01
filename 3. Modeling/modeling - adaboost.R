library(adabag)
library(caret)
library(e1071)
library(factoextra)
library(ggplot2)
library(devtools)
library(cluster)
library(dplyr)

#https://www.datatechnotes.com/2018/03/classification-with-adaboost-model-in-r.html#:~:text=AdaBoost%20(Adaptive%20Boosting)%20is%20a%20boosting%20algorithm%20in%20machine%20learning.&text=Adaboost%20improves%20those%20classifiers%20by,to%20classify%20data%20in%20R.

data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

#make Major, AIs in to Dummies

data$Major <- as.factor(data$Major)

data <- data[-c(1,2)] #remove PIN, Year
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set



model = boosting(Major~., data=train_data, boos=TRUE, mfinal=5)
pred = predict(model, test_data)

sum(diag(pred$confusion))/sum(pred$confusion)



cvmodel = boosting.cv(Major~., data=data, boos=TRUE, mfinal=10, v=7)
print(cvmodel[-1])

sum(diag(cvmodel$confusion))/sum(cvmodel$confusion)

########################################################################## just grades

data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

#make Major, AIs in to Dummies

data$Major <- as.factor(data$Major)

data <- data[-c(1,2,3,4)] #remove PIN, Year
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set



model = boosting(Major~., data=train_data, boos=TRUE, mfinal=5)
pred = predict(model, test_data)

sum(diag(pred$confusion))/sum(pred$confusion)



cvmodel = boosting.cv(Major~., data=data, boos=TRUE, mfinal=10, v=7)
print(cvmodel[-1])

sum(diag(cvmodel$confusion))/sum(cvmodel$confusion)


########################################################################## just AIs
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

#make Major, AIs in to Dummies

data$Major <- as.factor(data$Major)

data <- data[-c(1,2)] #remove PIN, Year
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set



model = boosting(Major~ AI1 + AI2, data=train_data, boos=TRUE, mfinal=5)
pred = predict(model, test_data)

sum(diag(pred$confusion))/sum(pred$confusion)



cvmodel = boosting.cv(Major~., data=data, boos=TRUE, mfinal=10, v=7)
print(cvmodel[-1])

sum(diag(cvmodel$confusion))/sum(cvmodel$confusion)


######################################################################### Division

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
#make Major, AIs in to Dummies

data$Major <- as.factor(data$Major)

data <- data[-c(1,2)] #remove PIN, Year
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set



model = boosting(Major~., data=train_data, boos=TRUE, mfinal=5)
pred = predict(model, test_data)

sum(diag(pred$confusion))/sum(pred$confusion)



cvmodel = boosting.cv(Major~., data=data, boos=TRUE, mfinal=10, v=7)
print(cvmodel[-1])

sum(diag(cvmodel$confusion))/sum(cvmodel$confusion)


########################################################################## unbalanced
library(adabag)
library(caret)
library(e1071)
library(factoextra)
library(ggplot2)
library(devtools)
library(cluster)
library(dplyr)

#https://www.datatechnotes.com/2018/03/classification-with-adaboost-model-in-r.html#:~:text=AdaBoost%20(Adaptive%20Boosting)%20is%20a%20boosting%20algorithm%20in%20machine%20learning.&text=Adaboost%20improves%20those%20classifiers%20by,to%20classify%20data%20in%20R.

data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

#make Major, AIs in to Dummies

data$Major <- as.factor(data$Major)

data <- data[-c(1,2)] #remove PIN, Year
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



cvmodel = boosting.cv(Major~., data=data_balanced_down, boos=TRUE, mfinal=10, v=7) #24.28
print(cvmodel[-1])

sum(diag(cvmodel$confusion))/sum(cvmodel$confusion)



cvmodel = boosting.cv(Major~., data=data_balanced_up, boos=TRUE, mfinal=10, v=7)
print(cvmodel[-1])

sum(diag(cvmodel$confusion))/sum(cvmodel$confusion) #35.5
