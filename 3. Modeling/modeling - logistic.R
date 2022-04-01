install.packages("rpart.plot")
install.packages("tree")
library(dplyr)
library(rpart)
library(rpart.plot)
library(tree)
library(caret)
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)


data <- data[-c(1,2)]
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

sum(test_data$AI1 == test_data$Major)/nrow(test_data)

#transform data

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



MCS.fit <- glm(Major == "MCS" ~., data= train_data, family = binomial)

LS.fit <- glm(Major == "LS" ~., data= train_data, family = binomial)

PSY.fit <- glm(Major == "PSY" ~., data= train_data, family = binomial)

PPE.fit <- glm(Major == "PPE" ~., data= train_data, family = binomial)

ES.fit <- glm(Major == "ES" ~., data= train_data, family = binomial)

PS.fit <- glm(Major == "PS" ~., data= train_data, family = binomial)

GA.fit <- glm(Major == "GA" ~., data= train_data, family = binomial)

HIST.fit <- glm(Major == "HIST" ~., data= train_data, family = binomial)

ANTH.fit <- glm(Major == "ANTH" ~., data= train_data, family = binomial)

ECON.fit <- glm(Major == "ECON" ~., data= train_data, family = binomial)

LIT.fit <- glm(Major == "LIT" ~., data= train_data, family = binomial)

ARTSHUM.fit <- glm(Major == "ARTSHUM" ~., data= train_data, family = binomial)

US.fit <- glm(Major == "US" ~., data= train_data, family = binomial)

PHIL.fit <- glm(Major == "PHIL" ~., data= train_data, family = binomial)


logistic_predictor <- function(test_data) {
  n <- nrow(test_data)
  predictor <- vector(mode = "character", length = n)
  for(i in 1:n) {
    prediction <- -10000
    major <- "empty"
    if (predict(MCS.fit, test_data[i,]) > prediction) {
    prediction <- predict(MCS.fit,test_data[i,]) ; major <- "MCS" }
    if (predict(ECON.fit, test_data[i,]) > prediction) {
    prediction <- predict(ECON.fit,test_data[i,]) ; major <- "ECON" }
    if (predict(LIT.fit, test_data[i,]) > prediction) {
    prediction <- predict(LIT.fit,test_data[i,]) ; major <- "LIT" }
    if (predict(LS.fit, test_data[i,]) > prediction) {
    prediction <- predict(LS.fit,test_data[i,]) ; major <- "LS" }
    if (predict(PSY.fit, test_data[i,]) > prediction) {
    prediction <- predict(PSY.fit,test_data[i,]) ; major <- "PSY" }
    if (predict(PPE.fit, test_data[i,]) > prediction) {
    prediction <- predict(PPE.fit,test_data[i,]) ; major <- "PPE" }
    if (predict(ES.fit, test_data[i,]) > prediction) {
    prediction <- predict(ES.fit,test_data[i,]) ; major <- "ES" }
    if (predict(PS.fit, test_data[i,]) > prediction) {
    prediction <- predict(PS.fit,test_data[i,]) ; major <- "PS" }
    if (predict(GA.fit, test_data[i,]) > prediction) {
    prediction <- predict(GA.fit,test_data[i,]) ; major <- "GA" }
    if (predict(HIST.fit, test_data[i,]) > prediction) {
    prediction <- predict(HIST.fit,test_data[i,]) ; major <- "HIST" }
    if (predict(ANTH.fit, test_data[i,]) > prediction) {
    prediction <- predict(ANTH.fit,test_data[i,]) ; major <- "ANTH" }
    if (predict(ARTSHUM.fit, test_data[i,]) > prediction) {
    prediction <- predict(ARTSHUM.fit,test_data[i,]) ; major <- "ARTS HUM" }
    if (predict(US.fit, test_data[i,]) > prediction) {
      prediction <- predict(US.fit,test_data[i,]) ; major <- "US" }
    if (predict(PHIL.fit, test_data[i,]) > prediction) {
      prediction <- predict(PHIL.fit,test_data[i,]) ; major <- "PHIL" }
    predictor[i] <- major
  }
  return(predictor)
}


logistic_prediction <- logistic_predictor(test_data)
print(logistic_prediction)
unique(logistic_prediction) #only predicts 11 major, ARTS HUM, LIT, ECON
unique(test_data$Major)

check <- as.data.frame(cbind(logistic_prediction, test_data$Major))
table(check)
confusionMatrix(logistic_prediction, test_data$Major)
accuracy_Test <- sum(logistic_prediction == test_data$Major) / nrow(test_data)
print(paste('Accuracy for test', accuracy_Test))


