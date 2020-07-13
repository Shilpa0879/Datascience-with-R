## HYPOTHESIS TESTING ASSIGNMENT

## LOAD DATA
MFratio<-read.csv("E:\\R-folder\\01-Assignments\\Hypothesis Testing\\BuyerRatio.csv")
str(MFratio)

Region<-c("East", "West", "North", "South")
Males<-c(50,142,131,70)
Females<-c(550,351,480,350)

MFRatio<-data.frame(Region,Males,Females)

t.test(Males,Females,alternative = "two.sided")



