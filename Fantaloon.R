## HYPOTHESIS TESTING ASSIGNMENT

library(stats)
library(dplyr)

## LOAD DATA
Fantaloon<-read.csv("E:\\R-folder\\01-Assignments\\Hypothesis Testing\\Faltoons.csv")
summary(Fantaloon)
str(Fantaloon)

table(Fantaloon$Weekdays, Fantaloon$Weekend)

boxplot(table(Fantaloon$Weekdays, Fantaloon$Weekend))

chisq.test(Fantaloon$Weekdays, Fantaloon$Weekend, correct = FALSE)
  

