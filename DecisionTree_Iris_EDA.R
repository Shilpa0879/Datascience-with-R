library(stats)
library(dplyr)

data(iris)

#Data Summarization----
summary(iris)
str(iris)

getmode<-function(x){
         uniq<-unique(x)
         uniq[which.max(tabulate(match(x,uniq)))]
}

x<-c(iris$Sepal.Length)
SL_mode<-getmode(x)
print(SL_mode)                          

x<-c(iris$Sepal.Width)
SW_mode<-getmode(x)
print(SW_mode)

x<-c(iris$Petal.Length)
PL_mode<-getmode(x)
print(PL_mode)

x<-c(iris$Petal.Width)
PW_mode<-getmode(x)
print(PW_mode)

x<-c(iris$Species)
SP_mode<-getmode(x)
print(SP_mode)

# Calculate Standard Deviation----
Std_Deviation<-c(sd(iris$Sepal.Length), sd(iris$Sepal.Width),
                 sd(iris$Petal.Length), sd(iris$Petal.Width))
print((Std_Deviation))

# Calculate Variance----
Variance<-c(var(iris$Sepal.Length), var(iris$Sepal.Width),
                 var(iris$Petal.Length), var(iris$Petal.Width))
print(Variance)

X1<-c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")

tab<-data.frame(X1, Std_Deviation, Variance)
tab

##Calculate IQR----
quantile(iris$Sepal.Length)
quantile(iris$Sepal.Width)
quantile(iris$Petal.Length)
quantile(iris$Petal.Width)

#Data Visualization----
#Numeric Data - Histogram
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)

#Categorical Data - Barplot
table(iris$Species)
barplot(table(iris$Species))

library(party)

str(iris)

set.seed(7)

train<-iris[1:112,]
test<-iris[113:150,]

iris_ctree<-ctree(train$Species~.,data = train)

testPred<-predict(iris_ctree, test, type='response')
testPred

a<-table(testPred,test$Species)
a
plot(iris_ctree)


