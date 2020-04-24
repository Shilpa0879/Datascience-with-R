#Load data
toyota1<-read.csv("E:\\R-folder\\CSV files\\Toyoto_Corrola.csv")

#Scatter plot matrix
pairs(toyota)
toyota<-toyota1[,-c(1,2,8)]

#Correlation matrix
cor(toyota)

#Regression Model and summary
Toyocar<-lm(Price~.,data = toyota)
summary(Toyocar)

#Multi-colinearity
install.packages("car") 
library(car)
car::vif(Toyocar)

#as Vif is less then 20 no need to do AIC


##Subset selection
library(MASS)
stepAIC(Toyocar)

#Toyocar<-lm(Price~.,data=Toyota)
#summary(Toyocar)

#Diagnostic Plots:
#Residual Plots, QQ-plos, Std. Resioduals vs Fitted
plot(Toyocar)

#Residuals vs Regressors
library(car)
residualPlots(Toyocar)

#Added Variable Plots
avPlots(Toyocar)

##QQPlots of studentized residuals
qqPlot(Toyocar)

#Deletion Diagnostics
influenceIndexPlot(Toyocar)

####Iteration 1
#Remove 3 observation 
Toyocar2<-toyota[-c(222,961,602),]  
Toyocar2['Age2']<-Toyocar2$Age_08_04*Toyocar2$Age_08_04

model.car1<-lm(Price~.,data = Toyocar2) 
summary(model.car1) 
car::vif(model.car1) 
plot(model.car1)  
residualPlots(model.car1) 
qqPlot(model.car1)
influenceIndexPlot(model.car1)
avPlots(model.car1)

#Remove 3 observation 
Toyocar3<-toyota[-c(222,524,602,961),]
Toyocar3['Age2']<-Toyocar3$Age_08_04*Toyocar3$Age_08_04

model.car2<-lm(Price~.,data=Toyocar3)
summary(model.car2)
car::vif(model.car2)
plot(model.car2)
residualPlots(model.car2)
qqPlot(model.car2)
influenceIndexPlot(model.car2)
avPlots(model.car2)

