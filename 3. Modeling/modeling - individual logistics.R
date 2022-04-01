library(dplyr)
library(rpart)
library(rpart.plot)
library(tree)
data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

#transform data

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI1, values_from = n, 
                     names_prefix = 'AI1_', values_fill = list(n = 0))

data <- data %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = AI2, values_from = n, 
                     names_prefix = 'AI2_', values_fill = list(n = 0))

data <- data[-c(1,2)]
n <- nrow(data)
ntrain <- round(n*0.75)  # 75% for training set
set.seed(11)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_data <- data[tindex,]   # Create training set
test_data <- data[-tindex,]   # Create test set

major <- unique(data$Major)
Accuracy <- NULL
Threshhold <- NULL

#MCS
train_data_MCS <- train_data
test_data_MCS <- test_data

train_data_MCS$Major[train_data_MCS$Major!= "MCS"] <- "Others"
test_data_MCS$Major[test_data_MCS$Major != "MCS"] <- "Others"

MCS.fit <- glm(Major == "MCS" ~., data= train_data_MCS, family = binomial)
MCS.probs <- predict(MCS.fit, type = "response")
glm.pred <- ifelse(MCS.probs > 0.5, "MCS", "Others")
table(glm.pred, train_data_MCS$Major)
mean(glm.pred == train_data_MCS$Major)

MCS.probs <- predict(MCS.fit, newdata = test_data_MCS, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(MCS.probs > cutoffs[i], "MCS", "Others")
  table(glm.pred,test_data_MCS$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_MCS$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression MCS", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)


#LS
train_data_LS <- train_data
test_data_LS <- test_data

train_data_LS$Major[train_data_LS$Major!= "LS"] <- "Others"
test_data_LS$Major[test_data_LS$Major != "LS"] <- "Others"

LS.fit <- glm(Major == "LS" ~., data= train_data_LS, family = binomial)
LS.probs <- predict(LS.fit, type = "response")
glm.pred <- ifelse(LS.probs > 0.5, "LS", "Others")
table(glm.pred, train_data_LS$Major)
mean(glm.pred == train_data_LS$Major)

LS.probs <- predict(LS.fit, newdata = test_data_LS, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(LS.probs > cutoffs[i], "LS", "Others")
  table(glm.pred,test_data_LS$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_LS$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression LS", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)



#PSY
train_data_PSY <- train_data
test_data_PSY <- test_data

train_data_PSY$Major[train_data_PSY$Major!= "PSY"] <- "Others"
test_data_PSY$Major[test_data_PSY$Major != "PSY"] <- "Others"

PSY.fit <- glm(Major == "PSY" ~., data= train_data_PSY, family = binomial)
PSY.probs <- predict(PSY.fit, type = "response")
glm.pred <- ifelse(PSY.probs > 0.5, "PSY", "Others")
table(glm.pred, train_data_PSY$Major)
mean(glm.pred == train_data_PSY$Major)

PSY.probs <- predict(PSY.fit, newdata = test_data_PSY, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(PSY.probs > cutoffs[i], "PSY", "Others")
  table(glm.pred,test_data_PSY$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_PSY$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression PSY", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)



#PPE
train_data_PPE<- train_data
test_data_PPE<- test_data

train_data_PPE$Major[train_data_PPE$Major!= "PPE"] <- "Others"
test_data_PPE$Major[test_data_PPE$Major != "PPE"] <- "Others"

PPE.fit <- glm(Major == "PPE" ~., data= train_data_PPE, family = binomial)
PPE.probs <- predict(PPE.fit, type = "response")
glm.pred <- ifelse(PPE.probs > 0.5, "PPE", "Others")
table(glm.pred, train_data_PPE$Major)
mean(glm.pred == train_data_PPE$Major)

PPE.probs <- predict(PPE.fit, newdata = test_data_PPE, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(PPE.probs > cutoffs[i], "PPE", "Others")
  table(glm.pred,test_data_PPE$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_PPE$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression PPE", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)



#ES
train_data_ES<- train_data
test_data_ES<- test_data

train_data_ES$Major[train_data_ES$Major!= "ES"] <- "Others"
test_data_ES$Major[test_data_ES$Major != "ES"] <- "Others"

ES.fit <- glm(Major == "ES" ~., data= train_data_ES, family = binomial)
ES.probs <- predict(ES.fit, type = "response")
glm.pred <- ifelse(ES.probs > 0.5, "ES", "Others")
table(glm.pred, train_data_ES$Major)
mean(glm.pred == train_data_ES$Major)

ES.probs <- predict(ES.fit, newdata = test_data_ES, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(ES.probs > cutoffs[i], "ES", "Others")
  table(glm.pred,test_data_ES$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_ES$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression ES", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)



#PS
train_data_PS<- train_data
test_data_PS<- test_data

train_data_PS$Major[train_data_PS$Major!= "PS"] <- "Others"
test_data_PS$Major[test_data_PS$Major != "PS"] <- "Others"

PS.fit <- glm(Major == "PS" ~., data= train_data_PS, family = binomial)
PS.probs <- predict(PS.fit, type = "response")
glm.pred <- ifelse(PS.probs > 0.5, "PS", "Others")
table(glm.pred, train_data_PS$Major)
mean(glm.pred == train_data_PS$Major)

PS.probs <- predict(PS.fit, newdata = test_data_PS, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(PS.probs > cutoffs[i], "PS", "Others")
  table(glm.pred,test_data_PS$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_PS$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression PS", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)



#GA
train_data_GA<- train_data
test_data_GA<- test_data

train_data_GA$Major[train_data_GA$Major!= "GA"] <- "Others"
test_data_GA$Major[test_data_GA$Major != "GA"] <- "Others"

GA.fit <- glm(Major == "GA" ~., data= train_data_GA, family = binomial)
GA.probs <- predict(GA.fit, type = "response")
glm.pred <- ifelse(GA.probs > 0.5, "GA", "Others")
table(glm.pred, train_data_GA$Major)
mean(glm.pred == train_data_GA$Major)

GA.probs <- predict(GA.fit, newdata = test_data_GA, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(GA.probs > cutoffs[i], "GA", "Others")
  table(glm.pred,test_data_GA$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_GA$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression GA", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)




#HIST
train_data_HIST<- train_data
test_data_HIST<- test_data

train_data_HIST$Major[train_data_HIST$Major!= "HIST"] <- "Others"
test_data_HIST$Major[test_data_HIST$Major != "HIST"] <- "Others"

HIST.fit <- glm(Major == "HIST" ~., data = train_data_HIST, family = binomial)
HIST.probs <- predict(HIST.fit, type = "response")
glm.pred <- ifelse(HIST.probs > 0.5, "HIST", "Others")
table(glm.pred, train_data_HIST$Major)
mean(glm.pred == train_data_HIST$Major)

HIST.probs <- predict(HIST.fit, newdata = test_data_HIST, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(HIST.probs > cutoffs[i], "HIST", "Others")
  table(glm.pred,test_data_HIST$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_HIST$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression HIST", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)



#ANTH
train_data_ANTH<- train_data
test_data_ANTH<- test_data

train_data_ANTH$Major[train_data_ANTH$Major!= "ANTH"] <- "Others"
test_data_ANTH$Major[test_data_ANTH$Major != "ANTH"] <- "Others"

ANTH.fit <- glm(Major == "ANTH" ~., data= train_data_ANTH, family = binomial)
ANTH.probs <- predict(ANTH.fit, type = "response")
glm.pred <- ifelse(ANTH.probs > 0.5, "ANTH", "Others")
table(glm.pred, train_data_ANTH$Major)
mean(glm.pred == train_data_ANTH$Major)

ANTH.probs <- predict(ANTH.fit, newdata = test_data_ANTH, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(ANTH.probs > cutoffs[i], "ANTH", "Others")
  table(glm.pred,test_data_ANTH$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_ANTH$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression ANTH", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)


#ECON
train_data_ECON<- train_data
test_data_ECON<- test_data

train_data_ECON$Major[train_data_ECON$Major!= "ECON"] <- "Others"
test_data_ECON$Major[test_data_ECON$Major != "ECON"] <- "Others"

ECON.fit <- glm(Major == "ECON" ~., data= train_data_ECON, family = binomial)
ECON.probs <- predict(ECON.fit, type = "response")
glm.pred <- ifelse(ECON.probs > 0.5, "ECON", "Others")
table(glm.pred, train_data_ECON$Major)
mean(glm.pred == train_data_ECON$Major)

ECON.probs <- predict(ECON.fit, newdata = test_data_ECON, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(ECON.probs > cutoffs[i], "ECON", "Others")
  table(glm.pred,test_data_ECON$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_ECON$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression ECON", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)



#LIT
train_data_LIT<- train_data
test_data_LIT<- test_data

train_data_LIT$Major[train_data_LIT$Major!= "LIT"] <- "Others"
test_data_LIT$Major[test_data_LIT$Major != "LIT"] <- "Others"

LIT.fit <- glm(Major == "LIT" ~., data= train_data_LIT, family = binomial)
LIT.probs <- predict(LIT.fit, type = "response")
glm.pred <- ifelse(LIT.probs > 0.5, "LIT", "Others")
table(glm.pred, train_data_LIT$Major)
mean(glm.pred == train_data_LIT$Major)

LIT.probs <- predict(LIT.fit, newdata = test_data_LIT, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(LIT.probs > cutoffs[i], "LIT", "Others")
  table(glm.pred,test_data_LIT$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_LIT$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression LIT", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)



#ARTS_HUM
train_data_ARTS_HUM<- train_data
test_data_ARTS_HUM<- test_data

train_data_ARTS_HUM$Major[train_data_ARTS_HUM$Major!= "ARTS HUM"] <- "Others"
test_data_ARTS_HUM$Major[test_data_ARTS_HUM$Major != "ARTS HUM"] <- "Others"

ARTS_HUM.fit <- glm(Major == "ARTS HUM" ~., data= train_data_ARTS_HUM, family = binomial)
ARTS_HUM.probs <- predict(ARTS_HUM.fit, type = "response")
glm.pred <- ifelse(ARTS_HUM.probs > 0.5, "ARTS HUM", "Others")
table(glm.pred, train_data_ARTS_HUM$Major)
mean(glm.pred == train_data_ARTS_HUM$Major)

ARTS_HUM.probs <- predict(ARTS_HUM.fit, newdata = test_data_ARTS_HUM, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(ARTS_HUM.probs > cutoffs[i], "ARTS HUM", "Others")
  table(glm.pred,test_data_ARTS_HUM$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_ARTS_HUM$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression ARTS_HUM", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)



#US
train_data_US<- train_data
test_data_US<- test_data

train_data_US$Major[train_data_US$Major!= "US"] <- "Others"
test_data_US$Major[test_data_US$Major != "US"] <- "Others"

US.fit <- glm(Major == "US" ~., data= train_data_US, family = binomial)
US.probs <- predict(US.fit, type = "response")
glm.pred <- ifelse(US.probs > 0.5, "US", "Others")
table(glm.pred, train_data_US$Major)
mean(glm.pred == train_data_US$Major)

US.probs <- predict(US.fit, newdata = test_data_US, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(US.probs > cutoffs[i], "US", "Others")
  table(glm.pred,test_data_US$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_US$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression US", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)



#PHIL
train_data_PHIL<- train_data
test_data_PHIL<- test_data

train_data_PHIL$Major[train_data_PHIL$Major!= "PHIL"] <- "Others"
test_data_PHIL$Major[test_data_PHIL$Major != "PHIL"] <- "Others"

PHIL.fit <- glm(Major == "PHIL" ~., data= train_data_PHIL, family = binomial)
PHIL.probs <- predict(PHIL.fit, type = "response")
glm.pred <- ifelse(PHIL.probs > 0.5, "PHIL", "Others")
table(glm.pred, train_data_PHIL$Major)
mean(glm.pred == train_data_PHIL$Major)

PHIL.probs <- predict(PHIL.fit, newdata = test_data_PHIL, type = "response")
cutoffs <- seq(0.05,0.95, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  glm.pred <- ifelse(PHIL.probs > cutoffs[i], "PHIL", "Others")
  table(glm.pred,test_data_PHIL$Major)
  accuracy <- c(accuracy, mean(glm.pred == test_data_PHIL$Major))
}

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression PHIL", xlab="Cutoff Level", ylab = "Accuracy %")
results <- as.data.frame(cbind(cutoffs, accuracy))
Accuracy <- c(Accuracy, max(accuracy))
Threshhold <- c(Threshhold,which.max(results$accuracy) * 0.05)

results <- as.data.frame(cbind(major, Accuracy, Threshhold))

