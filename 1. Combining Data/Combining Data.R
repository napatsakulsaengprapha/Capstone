#install packages
#install.packages("ggpubr")

#library
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(car)
library(dplyr)

x <- read.csv("allgrades_updated.csv", stringsAsFactors = FALSE)
x2 <- read.csv("majorandAI14-17and19.csv", stringsAsFactors = FALSE)


##
major <- read.csv("AnonMajors18.csv", stringsAsFactors = FALSE)
ai <- read.csv("AI.csv", stringsAsFactors = FALSE)

major$Major <- toupper(major$Major)
ai$Academic.Interest.1 <- toupper(ai$Academic.Interest.1)

major_and_ai_18 <- left_join(major, ai, by = "PIN")
major_and_ai_18 <- major_and_ai_18[, c(2,1,3,4)]

x2 <- rbind(x2, major_and_ai_18)

ddp <- subset(x2, x2$Major == "DDP LAW")

##

#remove Week 7
x <- subset(x, x$Module.Title != "Week 7: Experiential Learning Field Trip")


#remove CO23 SU sem grades to replace with another dataset
x <- x[!(x$Admit.Year == "2019" & x$Term.in.which.module.is.offered == "AY1920 S2"), ]

#remove semester class is offered
x <- select(x, -2)


#changing variable names
module_name_change <- function(Module.Title) {
  n <- length(Module.Title)
  x <- vector(mode = "character", length = n)
  for (i in 1:n){
    if (Module.Title[i] == " Literature and Humanities 2") {x[i] <- "Literature and Humanities 2"}
    else if (Module.Title[i] == "Philosophy and  Political Thought 2 ") {x[i] <- "Philosophy and Political Thought 2"}
    else if (Module.Title[i] == "Philosophy andPolitical Thought 2") {x[i] <- "Philosophy and Political Thought 2"}
    else if (Module.Title[i] == "Philosophy and  Political Thought 2") {x[i] <- "Philosophy and Political Thought 2"}
    else if (Module.Title[i] == "Philosophy and  Political Thought 1") {x[i] <- "Philosophy and Political Thought 1"}
    else if (Module.Title[i] == "Comparative Social Institutions") {x[i] <- "Comparative Social Inquiry"}
    else if (Module.Title[i] == "ARTS AND HUMANITIES") {x[i] <- "ARTS & HUMANITIES"}
    else {x[i] <- Module.Title[i]}
  }
  return(x)
}


x$Module.Title <- module_name_change(x$Module.Title)

#create new column to combine Shadow and Published Grades
final_grade <- function(Published.Grade, Shadow.Grade) {
  n <- length(Published.Grade)
  x <- vector(mode = "character", length = n)
  for (i in 1:n){
    if (Published.Grade[i] == "CS") {x[i] <- Shadow.Grade[i]}
    else if (Published.Grade[i] == "CU") {x[i] <- Shadow.Grade[i]}
    else {x[i] <- Published.Grade[i]}
  }
  return(x)
  }

x$Real.Grade <- final_grade(x$Published.Grade, x$Shadow.CC.Grades)

best <- subset(x, x$Real.Grade == "A+")

#remove shadow/published grade columns
x <- x[-c(4:5)]

#insert in CO23 SU SEM grade
CO19SUSEM <- read.csv("CO19SUSEM.csv", stringsAsFactors = FALSE)
x <- rbind(x, CO19SUSEM)


unique(x$Module.Title)
#
#SI into SI1
#FOS into SI2
#IS into SI2

unique(x$Real.Grade)
juniors <- subset(x, x$Admit.Year == 2019 & x$Real.Grade == "S")

p <- x %>% 
  select(PIN, Real.Grade)


#create new columns to change grade to numeric
unique(x$Real.Grade)
numeric_grade <- function(Real.Grade) {
  n <- length(Real.Grade)
  x <- vector(mode = "character", length = n)
  for (i in 1:n){
    if (Real.Grade[i] == "A+") {x[i] <- 5.5}
    else if (Real.Grade[i] == "A") {x[i] <- 5}
    else if (Real.Grade[i] == "A-") {x[i] <- 4.5}
    else if (Real.Grade[i] == "B+") {x[i] <- 4}
    else if (Real.Grade[i] == "B") {x[i] <- 3.5}
    else if (Real.Grade[i] == "B/B-") {x[i] <- 3.5}
    else if (Real.Grade[i] == "B ") {x[i] <- 3.5}
    else if (Real.Grade[i] == "B   " ) {x[i] <- 3.5}
    else if (Real.Grade[i] == "B-") {x[i] <- 3}
    else if (Real.Grade[i] == "C+") {x[i] <- 2.5}
    else if (Real.Grade[i] == "C") {x[i] <- 2}
    #else if (Real.Grade[i] == "C-") {x[i] <- 2}
    else if (Real.Grade[i] == "D+") {x[i] <- 1.5}
    else if (Real.Grade[i] == "D") {x[i] <- 1}
    #else if (Real.Grade[i] == "D-") {x[i] <- 0.5}
    else if (Real.Grade[i] == "F") {x[i] <- 0}
    else {x[i] <- 0} #need to account for S
  }
  return(as.numeric(x))
}

x$Real.Grade.Numeric <- numeric_grade(x$Real.Grade)

unique(x$Module.Title)

#reshape to wide data
y <- reshape(data = x, v.names = "Real.Grade.Numeric", timevar = "Module.Title", idvar = "PIN" , direction = "wide")
#some PIN does not have all 9 CC e.g. N371112425278


#joining data CC grade Data set & Major/Intended Major Data Set by PIN
z <- left_join(x2, y, by = "PIN")


#there is no variation in Major and AI1 for CO2022



#combining science grades

science <- z %>%
  select(`Real.Grade.Numeric.Scientific Inquiry`, `Real.Grade.Numeric.Scientific Inquiry 1`, `Real.Grade.Numeric.Scientific Inquiry 2`,
         `Real.Grade.Numeric.Foundations of Science`, `Real.Grade.Numeric.Foundations of Science 1`, `Real.Grade.Numeric.Foundations of Science 2`,
         `Real.Grade.Numeric.Integrated Science 1`, `Real.Grade.Numeric.Integrated Science 2`, `Real.Grade.Numeric.Integrated Science 3`)

science$Real.Grade.Numeric.Science <- rowMeans(science, na.rm = TRUE)

z$Real.Grade.Numeric.Science <- science$Real.Grade.Numeric.Science

clean_data <- z %>% 
  select(PIN ,Admit.Year, Academic.Interest.1, Academic.Interest.2, Major, `Real.Grade.Numeric.Literature and Humanities 1`, `Real.Grade.Numeric.Literature and Humanities 2`,
         `Real.Grade.Numeric.Philosophy and Political Thought 1`, `Real.Grade.Numeric.Philosophy and Political Thought 2`,
         `Real.Grade.Numeric.Comparative Social Inquiry`, `Real.Grade.Numeric.Modern Social Thought`, `Real.Grade.Numeric.Quantitative Reasoning`,
         `Real.Grade.Numeric.Science`)

tangmo <- subset(clean_data, clean_data$`Real.Grade.Numeric.Literature and Humanities 1` == 4.5 & clean_data$`Real.Grade.Numeric.Modern Social Thought` == 5.5)

CO22MCS <- subset(clean_data, clean_data$Admit.Year == "2018")

unique(clean_data$Major)
#remove data with no major/AIS
clean_data2 <- clean_data[!is.na(clean_data$Major), ]
                          
#& !is.na(clean_data$Academic.Interest.1) & !is.na(clean_data$Academic.Interest.2), ]
clean_data2 <- clean_data2[!is.na(clean_data2$Academic.Interest.1),]

clean_data3 <- clean_data2[ !(clean_data2$Major == "Undecided" | clean_data2$Major == "yet to declare" | clean_data2$Academic.Interest.1 == "Undecided"), ]

unique(clean_data3$Major)

major_name_change <- function(Major) {
  n <- length(Major)
  x <- vector(mode = "character", length = n)
  for (i in 1:n){
    if (Major[i] == "DDP LAW & PHILOSOPHY, POLITICS AND ECONOMICS Major") {x[i] <- "DDP LAW"}
    else if (Major[i] == "DDP LAW & PSYCHOLOGY Major") {x[i] <- "DDP LAW"}
    else if (Major[i] == "DDP LAW & GLOBAL AFFAIRS Major"    ) {x[i] <- "DDP LAW"}
    else if (Major[i] == "NOT APPLICABLE. I AM A DDP LAW STUDENT."    ) {x[i] <- "DDP LAW"} #combine all DDP Law into just DDP Law
    else if (Major[i] == "DDP Law"    ) {x[i] <- "DDP LAW"}
    else if (Major[i] == "ARTS AND HUMANITIES"    ) {x[i] <- "ARTS & HUMANITIES"}
    else {x[i] <- Major[i]}
  }
  return(x)
}

clean_data3$Major <- major_name_change(clean_data3$Major)

unique(clean_data3$Major)

#convert to upper letter
clean_data3$Academic.Interest.1 <- toupper(clean_data3$Academic.Interest.1)
clean_data3$Academic.Interest.2 <- toupper(clean_data3$Academic.Interest.2)
clean_data3$Major <- toupper(clean_data3$Major)

major_name_change <- function(Major) {
  n <- length(Major)
  x <- vector(mode = "character", length = n)
  for (i in 1:n){
    if (is.na(Major[i])) {x[i] <- NA}
    else if (Major[i] == "UNDECIDED") {x[i] <- NA}
    else if (Major[i] == "") {x[i] <- NA}
    else if (Major[i] == "PHILOSOPHY, POLITICS AND ECONOMICS") {x[i] <- "PPE"}
    else if (Major[i] == "ENVIRONMENTAL STUDIES") {x[i] <- "ES"}
    else if (Major[i] == "ARTS & HUMANITIES") {x[i] <- "ARTS HUM"}
    else if (Major[i] == "GLOBAL AFFAIRS") {x[i] <- "GA"}
    else if (Major[i] == "HISTORY") {x[i] <- "HIST"}
    else if (Major[i] == "PHILOSOPHY") {x[i] <- "PHIL"}
    else if (Major[i] == "DDP LAW") {x[i] <- "DDP"}
    else if (Major[i] == "ANTHROPOLOGY") {x[i] <- "ANTH"}
    else if (Major[i] == "ECONOMICS") {x[i] <- "ECON"}
    else if (Major[i] == "URBAN STUDIES") {x[i] <- "US"}
    else if (Major[i] == "LITERATURE") {x[i] <- "LIT"}
    else if (Major[i] == "PSYCHOLOGY") {x[i] <- "PSY"}
    else if (Major[i] == "MATHEMATICAL, COMPUTATIONAL AND STATISTICAL SCIENCES") {x[i] <- "MCS"}
    else if (Major[i] == "PHYSICAL SCIENCES") {x[i] <- "PS"}
    else if (Major[i] == "LIFE SCIENCES") {x[i] <- "LS"}
    else {x[i] <- Major[i]}
  }
  return(x)
}

clean_data3$Major <- major_name_change(clean_data3$Major)
clean_data3$Academic.Interest.1 <- major_name_change(clean_data3$Academic.Interest.1)
clean_data3$Academic.Interest.2 <- major_name_change(clean_data3$Academic.Interest.2)


unique(clean_data3$Major)
unique(clean_data3$Academic.Interest.1)
unique(clean_data3$Academic.Interest.2)


#remove DDP
clean_data3 <- clean_data3[clean_data3$Major != "DDP",]
clean_data3 <- clean_data3[!is.na(clean_data3$Academic.Interest.1),]

#export
unique(clean_data3$Academic.Interest.1)
unique(clean_data3$Academic.Interest.2)
unique(clean_data3$Major)

write.csv(clean_data3, "C:/Users/DELL/Desktop/Capstone/2. EDA/cleaned_data.csv", row.names=FALSE)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#How to deal with S?


