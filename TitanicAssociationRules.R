install.packages("arules")
library(arules)              

#Load data
Titanic<-read.csv("E:\\R-folder\\CSV files\\Titanic.csv")

Titanic<-Titanic[,c(-1)]
rules<-apriori(Titanic)
arules::inspect(rules)  
rules.sorted<-sort(rules,by="lift")
arules::inspect(rules.sorted)

#rules with rhs containing"survived" only

rules <- apriori(Titanic,parameter = list(supp=0.1, conf=0.5)
                 ,appearance = list(rhs=c("Survived=No", "Survived=Yes")
                 ),control = list(verbose=F))
arules::inspect(rules)



