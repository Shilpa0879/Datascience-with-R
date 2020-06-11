install.packages("randomForest")
library(randomForest)

model<-randomForest(iris$Species~.,data=iris,ntree=1000)

#View the forest results.
print(model)
#importance of the variable-Lower Gini
print(importance(model))
#prediction
pred<-predict(model,iris[,-5])
pred
table(pred,iris$Species)

##OOB :- Out of box, this is test data set 
## ntree is a function which do train and test by itself
