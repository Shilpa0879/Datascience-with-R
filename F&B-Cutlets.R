## HYPOTHESIS TESTING ASSIGNMENT
library(stats)
library(dplyr)

## LOAD DATA

Cutlets<-read.csv('E:\\R-folder\\Assignments\\Hypothesis Testing\\Cutlets.csv')

#Data Summarization
summary(Cutlets)

getmode<-function(x){
  uniq<-unique(x)
  uniq[which.max(tabulate(match(x,uniq)))]
}

x<-c(Cutlets$Unit.A)
UA_mode<-getmode(x)
print(UA_mode)

x<-c(Cutlets$Unit.B)
UB_mode<-getmode(x)
print(UB_mode)

#Calcualte Standard Deviation----
std_Deviation<-c(sd(Cutlets$Unit.A), sd(Cutlets$Unit.B))
print(std_Deviation)

#Calcualte Variance----
Variance<-c(var(Cutlets$Unit.A), var(Cutlets$Unit.B))
print(Variance)

X1<-c("Unit.A", 'Unit.B')

tab<-data.frame(X1,std_Deviation,Variance)
print(tab)

##Calculate IQR----
quantile(Cutlets$Unit.A)
quantile(Cutlets$Unit.B)

## Data Visualization----
#Numeric Data - Histogram
hist(Cutlets$Unit.A)
hist(Cutlets$Unit.B)

## Data Normalization----
CutletNum = select(Cutlets,c(1,2))
Zscore<-scale(CutletNum)

##______________________________________________

#Test perform:- Two Sample t-test
t.test(Cutlets$Unit.A,Cutlets$Unit.B,alternative = "two.sided")

boxplot(Cutlets$Unit.A,Cutlets$Unit.B)

