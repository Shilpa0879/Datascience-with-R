data("airquality")
head(airquality)
tail(airquality)
summary(airquality$Temp)
summary(airquality$Wind)
plot(airquality$Ozone)
#airquality=read.csv('path/airquality.csv',header=TRuE,sep=",")
plot(airquality$Ozone,airquality$Temp)
plot(airquality)



