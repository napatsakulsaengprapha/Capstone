#install.packages("ggpubr")
#install.packages("mice")
#install.packages("VIM")
#install.packages('Rcpp') 

#library
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(car)
library(circlize)
library(mice)
library(VIM)
library(Rcpp)

clean_data <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)

colnames(clean_data) <- c("PIN", "Admit.Year", "AI1", "AI2", "Major", "LH1", "LH2", "PPT1", "PPT2", "CSI", "MST", "QR", "Science")


#check if have SUSEM Admit 2019 has been added correctly - histogram should not have concentration at 0
younger <- subset(clean_data, clean_data$Admit.Year != "2014" & clean_data$Admit.Year != "2015")

hist(younger$LH1)
hist(younger$LH2)
hist(younger$PPT2)
hist(younger$Science)


#exploration and visualization
LH1 <- hist(clean_data$LH1, 
            xlim = c(0,6),
            breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
            main = "LH1 Grades",
            xlab = "Grades")

LH2 <- hist(clean_data$LH2, 
            xlim = c(0,6),
            breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
            main = "LH2 Grades",
            xlab = "Grades")

PPT1 <- hist(clean_data$PPT1, 
             xlim = c(0,6),
             breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
             main = "PPT1 Grades",
             xlab = "Grades")
  
PPT2 <- hist(clean_data$PPT2, 
             xlim = c(0,6),
             breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
             main = "PPT2 Grades",
             xlab = "Grades")

CSI <- hist(clean_data$CSI, 
            xlim = c(0,6),
            breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
            main = "CSI Grades",
            xlab = "Grades")

MST <- hist(clean_data$MST, 
            xlim = c(0,6),
            breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
            main = "MST Grades",
            xlab = "Grades")

QR <- hist(clean_data$QR, 
           xlim = c(0,6),
           breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
           main = "QR Grades",
           xlab = "Grades")

SCI <- hist(clean_data$Science, 
            xlim = c(0,6),
            breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
            main = "Science Grades",
            xlab = "Grades")

CO2022 <- subset(clean_data, clean_data$Admit.Year == "2018")
CO2022Moved <- subset(CO2022, CO2022$AI1 != CO2022$Major)


##Academic Interests
AI1<- ggplot(clean_data, aes(AI1)) + 
  geom_bar(stat = "count", fill = "navyblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

AI2 <-ggplot(clean_data, aes(AI2)) + 
  geom_bar(stat = "count", fill = "orange") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

Academic_Interests <- ggarrange(AI1, AI2,
                        labels = c("Academic Interest 1", "Academic Interest 2"),
                        ncol = 2, nrow = 1)
Academic_Interests

ggplot(clean_data, aes(Major)) + 
  geom_bar(stat = "count", fill = "navyblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Major")



#multivariate multiple regression vs. keras vs. multiple discriminant analysis

#separate AI vs. CC vs. Combined?

#multivariate multiple regression https://data.library.virginia.edu/getting-started-with-multivariate-multiple-regression/#:~:text=Multivariate%20Multiple%20Regression%20is%20the,single%20set%20of%20predictor%20variables.&text=It%20regresses%20each%20dependent%20variable%20separately%20on%20the%20predictors.

##separate data set into those with shadow and those withnot

older <- subset(clean_data, clean_data$Admit.Year == 2014 | clean_data$Admit.Year == 2015)
newer <- subset(clean_data, clean_data$Admit.Year != 2014 & clean_data$Admit.Year != 2015)

impute_model_mmr <- lm(cbind(LH1, PPT1, CSI)  
                   ~ LH2 + 
                     PPT2 + 
                     MST + 
                     QR +
                     Science, 
                   data = newer)

coef(impute_model_mmr)

Manova(impute_model_mmr)

summary(impute_model_mmr) #remove science?

predicted <- predict(impute_model_mmr, older)

anova(impute_model, impute_model2) #high P is evidence that the smaller model fits as well

impute_model3 <- linearHypothesis(impute_model, hypothesis.matrix = c("Science = 0"))
impute_model3 #confirms science is not different than 0


summary (rot.nls)


#use predicted

par(mfrow=c(3,2))

hist(predicted[,1],
     xlim = c(2,6),
     breaks = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "Predicted LH1 Grades",
     xlab = "Grades",
     col = "navyblue")

hist(clean_data$LH1[clean_data$LH1 > 0], 
     xlim = c(2,6),
     breaks = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "LH1 Grades",
     xlab = "Grades",
     col = "navyblue")

hist(predicted[,2],
     xlim = c(2,6),
     breaks = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "Predicted PPT1 Grades",
     xlab = "Grades",
     col = "navyblue")

hist(clean_data$PPT1[clean_data$PPT1 > 0], 
     xlim = c(2,6),
     breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "PPT1 Grades",
     xlab = "Grades",
     col = "navyblue")

hist(predicted[,3],
     xlim = c(2,6),
     breaks = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "Predicted CSI Grades",
     xlab = "Grades",
     col = "navyblue")

hist(clean_data$CSI[clean_data$CSI>0], 
     xlim = c(2,6),
     breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "CSI Grades",
     xlab = "Grades",
     col = "navyblue")

# update older with predicted value

older$LH1 <- predicted[,1]
older$PPT1 <- predicted[,2]
older$CSI <- predicted[,3]

complete_data <- rbind(older, newer)



#mice https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/

clean_data2 <- clean_data

clean_data2[clean_data2 == 0] <- NA

md.pattern(clean_data2)
mice_plot <- aggr(clean_data2, col=c('navyblue','orange'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(clean_data2), cex.axis=.7,
                    gap=1, ylab=c("Missing data","Pattern"))


mice_plot

imputed_model_mice <- mice(clean_data2, m=5, maxit = 50, method = 'pmm', seed = 500)

summary(imputed_Data)

completeData <- complete(imputed_model_mice, 1)


mice_plot <- aggr(completeData, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(clean_data2), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

predicted3 <- subset(completeData, completeData$Admit.Year == "2014" | completeData$Admit.Year == "2015")


par(mfrow=c(2,3))

hist(predicted3[,6],
     xlim = c(2,6),
     breaks = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "Predicted LH1 Grades",
     xlab = "Grades",
     col = "navyblue")

hist(clean_data$LH1[clean_data$LH1 > 0], 
     xlim = c(2,6),
     breaks = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "LH1 Grades",
     xlab = "Grades",
     col = "navyblue")

hist(predicted3[,8],
     xlim = c(2,6),
     breaks = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "Predicted PPT1 Grades",
     xlab = "Grades",
     col = "navyblue")

hist(clean_data$PPT1[clean_data$PPT1 > 0], 
     xlim = c(2,6),
     breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "PPT1 Grades",
     xlab = "Grades",
     col = "navyblue")

hist(predicted3[,10],
     xlim = c(2,6),
     breaks = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "Predicted CSI Grades",
     xlab = "Grades",
     col = "navyblue")

hist(clean_data$CSI[clean_data$CSI>0], 
     xlim = c(2,6),
     breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
     main = "CSI Grades",
     xlab = "Grades",
     col = "navyblue")


#exploration and visualization
par(mfrow=c(4,2))
LH1 <- hist(completeData$LH1, 
            xlim = c(0,6),
            breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
            main = "LH1 Grades",
            xlab = "Grades",
            col = "navyblue")

LH2 <- hist(completeData$LH2, 
            xlim = c(0,6),
            breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
            main = "LH2 Grades",
            xlab = "Grades",
            col = "navyblue")

PPT1 <- hist(completeData$PPT1, 
             xlim = c(0,6),
             breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
             main = "PPT1 Grades",
             xlab = "Grades",
             col = "navyblue")

PPT2 <- hist(completeData$PPT2, 
             xlim = c(0,6),
             breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
             main = "PPT2 Grades",
             xlab = "Grades",
             col = "navyblue")

CSI <- hist(completeData$CSI, 
            xlim = c(0,6),
            breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
            main = "CSI Grades",
            xlab = "Grades",
            col = "navyblue")

MST <- hist(completeData$MST, 
            xlim = c(0,6),
            breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
            main = "MST Grades",
            xlab = "Grades",
            col = "navyblue")

QR <- hist(completeData$QR, 
           xlim = c(0,6),
           breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
           main = "QR Grades",
           xlab = "Grades",
           col = "navyblue")

SCI <- hist(completeData$Science, 
            xlim = c(0,6),
            breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5),
            main = "Science Grades",
            xlab = "Grades",
            col = "navyblue")

var(completeData$QR)
var(completeData$Science)
var(completeData$MST)
#circle visualization

AI_to_Maj <- complete_data[, c(3,5)]
AI_to_Maj2 <- complete_data[, c(4,5)]


unique(clean_data$Major)
unique(clean_data$AI1) #cannot declare AI as DDP Law

mat = matrix(0, nrow = 14, ncol = 14)
rownames(mat) = c("PPE", "ES", "ARTS HUM", "GA",  "HIST", "PHIL", "ANTH", "ECON", "US", "LIT", "PSY", "MCS", "PS", "LS")
colnames(mat) = c("PPE", "ES", "ARTS HUM", "GA",  "HIST", "PHIL", "ANTH", "ECON", "US", "LIT", "PSY", "MCS", "PS", "LS")

adjacency_creator <- function(mat, AI, major){
  n <- length(major)
  m <- nrow(mat) # m = 16
  for (i in 1:m){ #row = AI
    for (j in 1:m){ #column = Maj
      for (k in 1:n){
        if (AI[k] == rownames(mat)[i] & major[k] == colnames(mat)[j]) {mat[i,j] = mat[i,j] + 1}
        }
    }
  }
  return(mat)
}

unique(AI_to_Maj$AI1)
unique(AI_to_Maj$Major)


#AI 1
mat2 <- adjacency_creator(mat, AI_to_Maj$AI1, AI_to_Maj$Major)

counter <- function(mat){
  x <- 0
  n <- nrow(mat)
  for (i in 1:n){
    for (j in 1:n){
      x <- x + mat[i,j]
    }
  }
  return(x)
}

counter(mat2)

par(mfrow=c(1,1))
chordDiagram(mat2) + title(main = "Academic Interest to Major Transitions")



mat3 <- adjacency_creator(mat, AI_to_Maj2$AI2, AI_to_Maj$Major)

counter(mat3)

par(mfrow=c(1,1))
chordDiagram(mat3) + title(main = "Academic Interest [2] to Major Transitions")

write.csv(completeData, "C:/Users/DELL/Desktop/Capstone/3. Modeling/final_data.csv", row.names=FALSE)

#MICE
#logistical regression for each major as EDA for model
#clustering?


#where should I do the imputation? the missing data for the older batch is SI. However, there are already 2 other data points. should we still impute?
#ignore
#SI for 2016 = SI1, different SI for 2014 2015
#better way to organize the R File? suggestions?
#stages: merge, clean, EDA