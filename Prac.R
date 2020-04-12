data("airquality")
head(airquality,10)
tail(airquality,10)
summary(airquality$Temp)
summary(airquality$Wind)
plot(airquality$Ozone)
#airquality=read.csv('path/airquality.csv',header=TRuE,sep=",")
plot(airquality$Ozone,airquality$Temp)
plot(airquality)

airquality[c(1,2)]

#Points and Lines
plot(airquality$Ozone, type="l") # p: points,l:lines, b:both

plot(airquality$Ozone, xlab='Ozone Concentration',
     ylab = 'No of Instance', main='Ozone in NY City', col="blue")
     
#Horizontal Bar Plot
barplot(airquality$Ozone, main='Ozone Concentration in air',
        xlab='Ozone levels', col="blue",horiz = F)

barplot(airquality$Temp, main='Temperature',
        xlab='Temperature levels', col="blue",horiz = F)

#Histogram
hist(airquality$Solar.R)
hist(airquality$Solar.R, 
     main ='Solar Radiation Values in air',
     xlab ='Solar rad.', col="blue")

summary(airquality$Solar.R)

#Single Box plot
boxplot(airquality$Solar.R)

#Multiple box plots
boxplot(airquality[,1:4], main='Multiple')

#Slicing the data frame
airquality[,c(1,3)]
head(airquality[,c(1,4)])
airquality$Ozone

airquality[1,c(1,3)]
airquality[c(1,2),c(1:6)]

#margin of the grid(mar),
#no of rows and columns(mfrow),
#whether a border is to be included(bty)
#and postion of the 
#labels(las: 1 for horizontal, las: 0 for vertical)
#bty - box around the plot 

par(mfrow=c(3,3),mar=c(2,5,2,1), las=1, bty="n")
plot(airquality$Ozone)
plot(airquality$Ozone, airquality$Wind)
plot(airquality$Ozone, type="l")
plot(airquality$Ozone, type="p")
plot(airquality$Ozone, type="b")
barplot(airquality$Ozone, main='Ozone Concentration in air',
        xlab= 'ozone levels', col='green',horiz = F)
hist(airquality$Solar.R)
boxplot(airquality$Solar.R)
boxplot(airquality[,0:4], main='Multiple Box plots')

        