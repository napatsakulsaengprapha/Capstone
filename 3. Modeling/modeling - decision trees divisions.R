install.packages("rpart.plot")
install.packages("tree")
install.packages("caret")

library(rpart)
library(rpart.plot)
library(tree)
require(tree)
library(caret)
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

n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

sum(test_data$AI1 == test_data$Major)/nrow(test_data)

##Decision Trees

#use all variables
fit <- rpart(Major~., data = train_data, method = 'class')
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


#optimization
fit <- rpart(Major~., data = train_data, method = 'class', control = rpart.control(cp=0.01))
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))



#just AI1
fit <- rpart(Major ~ AI1, data = train_data, method = 'class')
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


#just AI2
fit <- rpart(Major ~ AI2, data = train_data, method = 'class')
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

#AI1 + AI2
fit <- rpart(Major ~ AI1+ AI2, data = train_data, method = 'class')
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

#grades
fit <- rpart(Major ~ LH1 + LH2 + PPT1 + PPT2 + CSI + MST + QR + Science, data = train_data, method = 'class')
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

#use all variables changing depth
fit <- rpart(Major~., data = train_data, cp = 0)
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

##Random Forests https://www.datacamp.com/community/tutorials/decision-trees-R
install.packages("randomForest")
library(randomForest)
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)
data$Major <- major_to_division(data$Major)
data$AI1 <- major_to_division(data$AI1)
data$AI2 <- major_to_division(data$AI2)
data <- data[-c(1,2,4)]
data$Major <- as.factor(data$Major)
data$AI1 <- as.factor(data$AI1)
data$AI2 <- as.factor(data$AI2)
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

rf.major <- randomForest(Major~., data = train_data)
rf.major
summary(rf.major)
predict_major <- predict(rf.major, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

accuracy <- vector(mode = "numeric", length = 10)

for (mtry in 1:10){
  rf.major <- randomForest(Major~., data = train_data, mtry = mtry)
  rf.major
  summary(rf.major)
  predict_major <- predict(rf.major, test_data, type = 'class')
  table_mat <- table(test_data$Major, predict_major)           
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy[mtry] <- accuracy_Test
}

accuracy



#https://www.datatechnotes.com/2018/03/classification-with-gradient-boosting.html

library(gbm)

boost.major <- gbm(Major~., data = train_data, distribution = "multinomial", cv.folds = 10, n.trees = 250, shrinkage = 0.01, interaction.depth = 4)
summary(boost.major)
#plot(boost.major,i="AI1")
#plot(boost.major,i="QR")

predict_major <- predict.gbm(object = boost.major,
                   newdata = test_data,
                   n.trees = 250,
                   type = "response")

labels = colnames(predict_major)[apply(predict_major, 1, which.max)]
result = data.frame(test_data$Major, labels)

print(result)
cm = confusionMatrix(test_data$Major, as.factor(labels))
print(cm)

