# Load data
carpsw<-read.csv("E:\\R-folder\\Assignment\\Q7.csv")

car<-carpsw[,-1]
summary(car)
plot(car)

print(car)
mean(car$Points)
median(car$Score)
mode(car$Score)

mean1<-c(mean(car$Points),mean(car$Score), mean(car$Weigh))
print(mean1)

median1<-c(median(car$Points), median(car$Score),median(car$Weigh))
median1

var(car$Points)
var(car$Score)

sd(car$Points)
sd(car$Score)
sd(car$Weigh)

range(car$Points)
range(car$Score)



























































































































