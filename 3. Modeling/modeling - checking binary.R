library(dynamicTreeCut)
library(dendextend)
library(factoextra)
library(ggplot2)
library(devtools)
library(cluster)
library(dplyr)

#https://www.baeldung.com/cs/clustering-unknown-
#DBSCAN and Mean Shift have issue in high dimension
#spectral clustering reliance on k-mean in final step


#https://www.analyticsvidhya.com/blog/2019/05/beginners-guide-hierarchical-clustering/
#https://academic.oup.com/bioinformatics/article/24/5/719/200751?login=true


data<- read.csv("final_data.csv", stringsAsFactors = FALSE)

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


data <- data[-c(1,2,3)] #remove PIN, Year (unsupervised)

major_labels <- data[,1]


data <- as.data.frame(scale(data[,-1])) #run after below


dist_mat <- dist(data, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
hclust_avg$height <- round(hclust_avg$height, 6)
plot(hclust_avg)

cutoff <- cutreeDynamic(hclust_avg, method = "tree", deepSplit = FALSE)
max(cutoff) #17, other number if use other method for hclust

plot(hclust_avg)
rect.hclust(hclust_avg , k = 14, border = 2:6)
abline(h = 14, col = 'red')

avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, k = 14)
plot(avg_col_dend)

labels <- cbind(major_labels, cutoff)




one <- subset(labels, labels$cutoff == 1)
ggplot(one, aes(x = Major)) +
  geom_bar() +
  ggtitle("Group 1 Major Distribution")
#MCS/ANTH


two <- subset(labels, labels$cutoff == 2)
ggplot(two, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 2 Major Distribution")
#ES/US


three <- subset(labels, labels$cutoff == 3)
ggplot(three, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 3 Major Distribution")
#ES


four <- subset(labels, labels$cutoff == 4)
ggplot(four, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 4 Major Distribution")
#GA/ARTSHUM


five <- subset(labels, labels$cutoff == 5)
ggplot(five, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 5 Major Distribution")
#MCS


six <- subset(labels, labels$cutoff == 6)
ggplot(six, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 6 Major Distribution")
#PPE/GA



seven <- subset(labels, labels$cutoff == 7)
ggplot(two, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 7 Major Distribution")
#ES/US


eight <- subset(labels, labels$cutoff == 8)
ggplot(eight, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 8 Major Distribution")
#PPE
#relatively small though

nine <- subset(labels, labels$cutoff == 9)
ggplot(nine, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 9 Major Distribution")
#PSY/LS


ten <- subset(labels, labels$cutoff == 10)
ggplot(ten, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 10 Major Distribution")
#ECON


eleven <- subset(labels, labels$cutoff == 11)
ggplot(eleven, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 11 Major Distribution")
#PPE


twelve <- subset(labels, labels$cutoff == 12)
ggplot(twelve, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 12 Major Distribution")
#mix of everything with HIST


thirteen <- subset(labels, labels$cutoff == 13)
ggplot(thirteen, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 13 Major Distribution")
#PSY


fourteen <- subset(labels, labels$cutoff == 14)
ggplot(fourteen, aes(x = Major)) +
  geom_bar()+
  ggtitle("Group 14 Major Distribution")
#PPE

ggplot(labels, aes(x = cutoff)) +
  geom_bar()



