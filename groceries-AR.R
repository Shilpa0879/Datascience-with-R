#Load Data
grocery<-read.transactions("E:\\R-folder\\01-Assignments\\Association Rules\\groceries.csv",format = "basket")

summary(grocery)

library(arules)
#install.packages("arulesViz")
library(arulesViz)

head(grocery)
apriori(grocery)

itemFrequencyPlot(grocery, topN=20, type = "absolute")

#rules with support = 0.006, confidence = 0.25
rules<-apriori(grocery, parameter = list(supp=0.006, conf=0.25, minlen=2))
arules::inspect(rules[1:10])
rules.sort<-sort(rules, by = "lift")[1:5]
inspect(rules.sort[1:5])

plot(rules, method = "scatterplot")
plot(rules, method = "grouped")
windows()
plot(rules, method = "mosaic")


#rules with support = 0.005, confidence = 0.8
groceryrules1 <- apriori(grocery, parameter = list(supp=0.005, conf=0.8,minlen=2))
options(digits = 2)
arules::inspect(groceryrules1)
rules.sort1<-sort(groceryrules1, by="lift")
inspect(rules.sort1)
windows()
plot(groceryrules1, method = "scatterplot")
plot(groceryrules1, method = "grouped")

#rules with support = 0.003, confidence = 0.7
groceryrules2 <- apriori(grocery, parameter = list(supp=0.001, conf=0.8), 
                         apperance = list(default = "lhs", rhs = "bakery"), 
                         control = list(verbose = F))

groceryrules2<-apriori(grocery, parameter = list(supp=0.001, conf=0.8), 
                       appearance = list(default="lhs", rhs="bakery"),
                       control = list(verbose=F))
arules::inspect(groceryrules2[1:10])
plot(groceryrules2, method = "scatterplot")
plot(groceryrules2, method = "grouped")
