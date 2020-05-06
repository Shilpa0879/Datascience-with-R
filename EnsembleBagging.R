data('iris')
install.packages("ipred")
library(ipred)

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
