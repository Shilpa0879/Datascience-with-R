#Load data 
cigarette_consumption<-read.csv("C:\\Users\\user\\Desktop\\Cigarttes.csv")

#Scatter Plot Matrix:
pairs(cigarette_consumption[,2:8])

#Correlation
cor(cigarette_consumption[,2:8])
#Regression Model:
reg.model<-lm(Sales~Age+HS+Income+Black+Female+Price,data=cigarette_consumption)
summary(reg.model)
