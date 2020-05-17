# Load data
Company<-read.csv("E:\\R-folder\\Assignments\\Decision Tree Assignment\\Company_Data.csv")

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
