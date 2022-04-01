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
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

sum(test_data$AI1 == test_data$Major)/nrow(test_data) #0.3465704

##Decision Trees

#use all variables
fit <- rpart(Major~., data = train_data, method = 'class')
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test)) #beat by a little bit


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
table_mat
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


sum(data$AI1 == data$Major)/nrow(data)

install.packages("gbm") 

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






#################################################################################### just grades

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
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

sum(test_data$AI1 == test_data$Major)/nrow(test_data) #0.3465704

##Decision Trees

#use all variables
fit <- rpart(Major~., data = train_data, method = 'class')
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test)) #beat by a little bit


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
#install.packages("randomForest")
library(randomForest)
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)
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

rf.major <- randomForest(Major ~ LH1 + LH2 + PPT1 + PPT2 + CSI + MST + QR + Science, data = train_data)
rf.major
summary(rf.major)
predict_major <- predict(rf.major, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

accuracy <- vector(mode = "numeric", length = 10)

for (mtry in 1:10){
  rf.major <- randomForest(Major ~ LH1 + LH2 + PPT1 + PPT2 + CSI + MST + QR + Science, data = train_data, mtry = mtry)
  rf.major
  summary(rf.major)
  predict_major <- predict(rf.major, test_data, type = 'class')
  table_mat <- table(test_data$Major, predict_major)           
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy[mtry] <- accuracy_Test
}


accuracy


install.packages("gbm") 

#https://www.datatechnotes.com/2018/03/classification-with-gradient-boosting.html

library(gbm)

boost.major <- gbm(Major ~ LH1 + LH2 + PPT1 + PPT2 + CSI + MST + QR + Science, data = train_data, distribution = "multinomial", cv.folds = 10, n.trees = 250, shrinkage = 0.01, interaction.depth = 4)
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

#use clustering? to discover groups? k-means? 
#cluster into sections? science, social sciences, humanities? 14 majors?



#################################################################################### AI1 + AI2
library(randomForest)
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)
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

rf.major <- randomForest(Major ~ AI1, data = train_data)
rf.major
summary(rf.major)
predict_major <- predict(rf.major, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

accuracy <- vector(mode = "numeric", length = 10)

for (mtry in 1:10){
  rf.major <- randomForest(Major ~ AI1, data = train_data, mtry = mtry)
  rf.major
  summary(rf.major)
  predict_major <- predict(rf.major, test_data, type = 'class')
  table_mat <- table(test_data$Major, predict_major)           
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy[mtry] <- accuracy_Test
}


accuracy



library(gbm)
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)
data <- data[-c(1,2)]
data$Major <- as.factor(data$Major)
data$AI1 <- as.factor(data$AI1)
data$AI2 <- as.factor(data$AI2)
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,] 


#, cv.folds = 10, n.trees = 250, shrinkage = 0.01, interaction.depth = 4
boost.major <- gbm(Major ~ AI1 + AI2, data = train_data, distribution = "multinomial", cv.folds = 10, n.trees = 250, shrinkage = 0.01, interaction.depth = 4)
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



#################################################################################################### unbalanced fix
#install.packages("ROSE")
library(rpart)
library(ROSE)
library(rpart.plot)
library(tree)
require(tree)
library(caret)
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

data <- data[-c(1,2)]
data$AI1 <- as.factor(data$AI1)
data$AI2 <- as.factor(data$AI2)
data$Major <- as.factor(data$Major)
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set


sum(test_data$AI1 == test_data$Major)/nrow(test_data) #0.3465704
is.factor(data$Major)
data_balanced_down <- downSample(train_data[,-3], train_data$Major, list = FALSE, yname = "Major")


##Decision Trees

#use all variables
fit <- rpart(Major~., data = data_balanced_down, method = 'class')
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test)) #30%



sum(test_data$AI1 == test_data$Major)/nrow(test_data) #0.3465704
is.factor(data$Major)
data_balanced_up <- upSample(train_data[,-3], train_data$Major, list = FALSE, yname = "Major") #26%

##gbm 24.91%
boost.major <- gbm(Major~., data = data_balanced_down, distribution = "multinomial", cv.folds = 10, n.trees = 250, shrinkage = 0.01, interaction.depth = 4)
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

##
#random forest #23.4
data_balanced_down <- data_balanced_down[-c(2)]
data_balanced_down$AI1 <- as.factor(data_balanced_down$AI1)
data_balanced_down$AI2 <- as.factor(data_balanced_down$AI2)
data_balanced_down$Major <- as.factor(data_balanced_down$Major)

rf.major <- randomForest(Major~., data = data_balanced_down)
rf.major
summary(rf.major)
predict_major <- predict(rf.major, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

accuracy <- vector(mode = "numeric", length = 10)

for (mtry in 1:10){
  rf.major <- randomForest(Major~., data = data_balanced_down, mtry = mtry)
  rf.major
  summary(rf.major)
  predict_major <- predict(rf.major, test_data, type = 'class')
  table_mat <- table(test_data$Major, predict_major)           
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy[mtry] <- accuracy_Test
}

accuracy

##Decision Trees

#use all variables
fit <- rpart(Major~., data = data_balanced_up, method = 'class')
rpart.plot(fit)

predict_major <- predict(fit, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)           
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test)) #28%


#gbm

boost.major <- gbm(Major~., data = data_balanced_up, distribution = "multinomial", cv.folds = 10, n.trees = 250, shrinkage = 0.01, interaction.depth = 4)
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


#random forest #27.4
data_balanced_up <- data_balanced_up[-c(2)]
data_balanced_up$AI1 <- as.factor(data_balanced_up$AI1)
data_balanced_up$AI2 <- as.factor(data_balanced_up$AI2)
data_balanced_up$Major <- as.factor(data_balanced_up$Major)

rf.major <- randomForest(Major~., data = data_balanced_up)
rf.major
summary(rf.major)
predict_major <- predict(rf.major, test_data, type = 'class')
table_mat <- table(test_data$Major, predict_major)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

accuracy <- vector(mode = "numeric", length = 10)

for (mtry in 1:10){
  rf.major <- randomForest(Major~., data = data_balanced_up, mtry = mtry)
  rf.major
  summary(rf.major)
  predict_major <- predict(rf.major, test_data, type = 'class')
  table_mat <- table(test_data$Major, predict_major)           
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy[mtry] <- accuracy_Test
}

accuracy

