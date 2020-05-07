data('iris')
install.packages("ipred")
library(ipred)
##Bagging
set.seed(300)

mybag<-bagging(iris$Species~.,data = iris,nbagg = 50)
mybag
credit_pred<-predict(mybag,iris)
credit_pred
table(credit_pred,iris$Species)
table(credit_pred,iris$Sepal.Length)

train<-iris[1:130,]
test<-iris[131:150,]
mybag<-bagging(train$Species~.,data = train,nbagg = 50)
mybag

credit_pred<-predict(mybag,test[,-5])
credit_pred
table(credit_pred,test$Species)

mybag<-bagging(test$Species~.,data = test,nbagg = 50)
mybag
credit_pred<-predict(mybag,train[,-5])
credit_pred
table(credit_pred,train$Species)

##Boosting
#Data partition for model building and testing
library(caret)
inTraininglocal<-createDataPartition(iris$Species,p=.70,list=F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]

#Model Building
library(C50)
model<-C5.0(training$Species~.,data=training,trials=20)  #Trails Boosting parameter
model
#Generate the model sumamry
summary(model)
#Predict for test data set
pred<-predict.C5.0(model,testing[,-5])
pred
a<-table(testing$Species,pred)
a
sum(diag(a))/sum(a)

