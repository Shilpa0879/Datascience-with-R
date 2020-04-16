#Load data 
Nd<-read.csv("C:\\Users\\user\\Desktop\\NewspaperData.csv")

#Visualization
install.packages("Lattice")
library(lattice)
dotplot(Nd$sunday, main="Dot plot of Sunday circulation",
        col="dodgerblue4")
dotplot(Nd$sunday, main="Dot plot of Daily circulations",
        col="dodgerblue4")
boxplot(Nd$sunday,main="Dot plot of Sunday circulation",col="dodgerblue4")
boxplot(Nd$daily,main="Dot plot of Daily circulations",col="dodgerblue4")

#Regression equation
#Syntax  model<-lm(y~x,data=data set name)
colnames(Nd)
model<-lm(sunday~daily,data=Nd)

summary(model)
sun=13.84+(1.34*200)

pred<-predict(model)

#final<-data.frame(Nd,"pred"=pred,"Error"=Nd$sunday~pred)

#model<-lm(AT~Waist,data =WC_AT)

summary(model)

boxplot(Nd[,1:3], main='Multiple')








