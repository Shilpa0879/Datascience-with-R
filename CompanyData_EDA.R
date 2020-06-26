install.packages("stats")
install.packages("dplyr")

library(stats)
library(dplyr)

# Load data-----
Company<-read.csv("E:\\R-folder\\01-Assignments\\Decision Tree Assignment\\Company_Data.csv")
str(Company)

#Data Summarization-----
summary(Company)


getmode<-function(x){
  uniq<-unique(x)
  uniq[which.max(tabulate(match(x,uniq)))]
}

x<-c(Company$Sales)
Sales_mode<-getmode(x)
print(Sales_mode)

x<-c(Company$CompPrice)
CompPrice_mode<-getmode(x)
print(CompPrice_mode)

x<-c(Company$Income)
Income_mode<-getmode(x)
print(Income_mode)

x<-c(Company$Advertising)
Advertising_mode<-getmode(x)
print(Advertising_mode)

x<-c(Company$Population)
Population_mode<-getmode(x)
print(Population_mode)

x<-c(Company$Income)
Income_mode<-getmode(x)
print(Income_mode)

x<-c(Company$Price)
Price_mode<-getmode(x)
print(Price_mode)

x<-c(Company$ShelveLoc)
Shelveloc_mode<-getmode(x)
print(Shelveloc_mode)

x<-c(Company$Age)
Age_mode<-getmode(x)
print(Age_mode)

x<-c(Company$Education)
Education_mode<-getmode(x)
print(Education_mode)

x<-c(Company$Urban)
Urban_mode<-getmode(x)
print(Urban_mode)

x<-c(Company$US)
US_mode<-getmode(x)
print(US_mode)

# Calculate Standard Deviation----
Std_deviation<-c(sd(Company$Sales), sd(Company$CompPrice), sd(Company$Income),
                 sd(Company$Advertising), sd(Company$Population),sd(Company$Price),
                 sd(Company$Age),sd(Company$Education))
print(Std_deviation)                 

# Calculate Variance----
Variance<-c(var(Company$Sales), var(Company$CompPrice), var(Company$Income),
                 var(Company$Advertising), var(Company$Population),var(Company$Price),
                 var(Company$Age),var(Company$Education))
print(Variance)                 

tab<-data.frame(Std_deviation,Variance)

##Calculate IQR----
quantile(Company$Sales)
quantile(Company$CompPrice)
quantile(Company$Income)
quantile(Company$Advertising)
quantile(Company$Population)
quantile(Company$Price)
quantile(Company$Age)
quantile(Company$Education)

## Data Visualization----
#Numeric Data - Histogram
hist(Company$Sales)
hist(Company$CompPrice)
hist(Company$Income)
hist(Company$Advertising)
hist(Company$Population)
hist(Company$Price)
hist(Company$Age)
hist(Company$Education)

#Categorical Data - Barplot
table(Company$ShelveLoc)
barplot(table(Company$ShelveLoc))

table(Company$Urban)
barplot(table(Company$Urban))

table(Company$US)
barplot(table(Company$US))

## Data Normalization----
CompanyNum = select(Company,c(1,2,3,4,5,6,8,9))
ZScore<-scale(CompanyNum)

##______________________________________________

install.packages("ISLR")
install.packages("tree")

rm(list=ls())
library(ISLR)
library(tree)

# Creating categorical variable for sales attribute

High=ifelse(Company$Sales>=8, "Yes","No")
CompanyData<-data.frame(Company,High)

head(CompanyData)

CompanyData=CompanyData[,-1]

set.seed(7)
traindata=sample(1:nrow(CompanyData),nrow(CompanyData)/2)
test=-traindata
test      

training_data<-CompanyData[traindata,]
testing_data<-CompanyData[test,]
testing_high<-High[test]

tree_model=tree(High~.,training_data)
plot(tree_model)
text(tree_model, pretty=0)

tree_pred=predict(tree_model, testing_data, type="class")
tree_pred    

a<-table(testing_high,tree_pred)
sum(diag(a))/sum(a)

