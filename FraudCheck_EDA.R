library(stats)
library(dplyr)

# Load data
TaxableData<-read.csv("E:\\R-folder\\Assignments\\Decision Tree Assignment\\Fraud_check.csv")

#Data Summarization-----
summary(TaxableData)

getmode<-function(x){
         uniq<-unique(x)
         uniq[which.max(tabulate(match(x,uniq)))]
}

x<-c(TaxableData$Undergrad)
Undergrad_mode<-getmode(x)
print(Undergrad_mode)

x<-c(TaxableData$Marital.Status)
MS_mode<-getmode(x)
print(MS_mode)

x<-c(TaxableData$Taxable.Income)
TI_mode<-getmode(x)
print(TI_mode)

x<-c(TaxableData$City.Population)
CP_mode<-getmode(x)
print(CP_mode)

x<-c(TaxableData$Work.Experience)
WE_mode<-getmode(x)
print(WE_mode)

x<-c(TaxableData$Urban)
Urban_mode<-getmode(x)
print(Urban_mode)

# Calculate Standard Deviation----
Std_deviation<-c(sd(TaxableData$Taxable.Income), sd(TaxableData$City.Population), 
                 sd(TaxableData$Work.Experience))
print(Std_deviation)                 

# Calculate Variance----
Variance<-c(var(TaxableData$Taxable.Income), var(TaxableData$City.Population), 
                 var(TaxableData$Work.Experience))
print(Variance)    

X1<-c("Taxable.Income", "City.Population", "Work.Experience")

tab<-data.frame(X1,Std_deviation,Variance)
print(tab)

##Calculate IQR----
quantile(TaxableData$Taxable.Income)
quantile(TaxableData$City.Population)
quantile(TaxableData$Work.Experience)

## Data Visualization----
#Numeric Data - Histogram
hist(TaxableData$Taxable.Income)
hist(TaxableData$City.Population)
hist(TaxableData$Work.Experience)

#Categorical Data - Barplot
table(TaxableData$Undergrad)
barplot(table(TaxableData$Undergrad))

table(TaxableData$Marital.Status)
barplot(table(TaxableData$Marital.Status))

table(TaxableData$Work.Experience)
barplot(table(TaxableData$Work.Experience))

## Data Normalization----
TaxNum = select(TaxableData,c(3,4,5))
ZScore<-scale(TaxNum)

##______________________________________________


library(ISLR)
library(tree)

#Creating condition for taxable income

Tax=ifelse(TaxableData$Taxable.Income<=30000, "Risky","Good")
TaxData<-data.frame(TaxableData,Tax)

TaxData=TaxData[,-3]

set.seed(7)
traindata=sample(1:nrow(TaxData), nrow(TaxData)/2)
test=-traindata

training<-TaxData[traindata,]
testing<-TaxData[test,]
test_tax<-Tax[test]

tree_model<-tree(Tax~.,training)
summary(tree_model)
plot(tree_model)
text(tree_model, pretty=0)

predtax=predict(tree_model,testing, type="class")
predtax

a<-table(test_tax,predtax)
sum(diag(a)/sum(a))

hist(TaxableData$Taxable.Income)

