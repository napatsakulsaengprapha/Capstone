library(dplyr)

major <- read.csv("Juniors_Major.csv", stringsAsFactors = FALSE)
AI <- read.csv("Juniors_AI.csv", stringsAsFactors = FALSE)

names(AI)[names(AI) == 'ï..PIN'] <- 'PIN'

juniors <- full_join(major, AI, by = "PIN")


write.csv(juniors, "C:/Users/DELL/Desktop/Capstone/Juniors_fina..csv", row.names=FALSE)
