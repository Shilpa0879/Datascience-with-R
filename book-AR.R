#Load Data
book<-read.csv("E:\\R-folder\\01-Assignments\\Association Rules\\book.csv")
summary(book)
str(book)

library(arules)
library(arulesViz)
rules<-apriori(as.matrix(book))
arules::inspect(rules)
rules<-sort(rules, by="lift")
inspect(rules)

#rules with support = 0.02, confidence = 0.5
bookrules<-apriori(as.matrix(book), parameter = list(support = 0.02, confidence = 0.5, 
                   minlen=5))
arules::inspect(bookrules)
rules.sorted<-sort(bookrules, by="lift")
inspect(rules.sorted[1:10])
plot(bookrules, method = "scatterplot")
plot(bookrules, method = "grouped")


#rules with support = 0.04, confidence = 0.8
bookrules1<-apriori(as.matrix(book), parameter = list(supp=0.04, conf=0.8),
                    appearance = list(default="lhs", rhs="ChildBks"),
                    control = list(verbose=F))
arules::inspect(bookrules1)
rules.sorted1<-sort(bookrules1, by="confidence")
inspect(rules.sorted1)
plot(bookrules1, method = "scatterplot")
plot(bookrules1, method = "grouped")

#rules with support = 0.05, confidence = 0.8
bookrules2<-apriori(as.matrix(book), parameter = list(supp=0.05, conf=0.8),
                    appearance = list(default="lhs", rhs="ChildBks"),
                    control = list(verbose=F))
arules::inspect(bookrules2)
rules.sorted2<-sort(bookrules2, by="lift")
inspect(rules.sorted2)
plot(bookrules2, method = "scatterplot")
plot(bookrules2, method = "grouped")


