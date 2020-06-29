library(forecast)
library(fpp) 
library(smooth)
library(tseries) 
library(readxl)
library(stats)
library(dplyr)

# Loading CocaCola Sales Data 
Cocacola<-read_xlsx(file.choose())

#Data Summarization
summary(Cocacola)

getmode<-function(x){
         uniq=unique(x)
         uniq[which.max(tabulate(match(x,uniq)))]
}

x<-c(Cocacola$Sales)
Sales_mode<-getmode(x)
print(Sales_mode)
sd(Cocacola$Sales)
var(Cocacola$Sales)

hist(Cocacola$Sales)

##___________________________________________

#Converting Data into Time Series object
amts<-ts(Cocacola$Sales, frequency = 4, start = c(86))
amts
plot(amts)

# dividing the entire data into training and testing data
train<-amts[1:38]
test<-amts[39:42]

#Converting Time series object
train<-ts(train, frequency = 4)
test<-ts(test, frequency = 4)

#Plotting Time Series data
plot(train)

##___________________________________________

##Using HoltWinters function________
#Assuming Time Series data having only level parameter, where alpha=02
hw_a<-HoltWinters(train, alpha = 0.2, beta = F, gamma = F)
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))

plot(forecast(hw_a,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100

##___________________________________________

#By looking at the plot the forecasted values are not showing any characters of
#train data
#Assuming the Time Series data has level and trend parameter
#Considering alpha = 0.2, beta = 0.1, gamma = F
hw_ab<-HoltWinters(train, alpha = 0.2, beta = 0.1, gamma = F)
hwab_pred<-data.frame(predict(hw_ab, n.ahead = 4))

plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(hwab_pred$fit, test)*100

##___________________________________________

#Assuming the Time Series data has level and trend parameter
#Considering alpha = 0.2, beta = 0.1, gamma = 0.1
hw_abg<-HoltWinters(train, alpha = 0.2, beta = 0.1, gamma = 0.1)
hwabg_pred<-data.frame(predict(hw_abg, n.ahead = 4))

plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100

##___________________________________________

hw_nabg<-HoltWinters(train)
hwnabg_pred<-data.frame(predict(hw_nabg ,n.ahead=4))

plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100

##___________________________________________

#Data level, trend, seasonality characters with default value of alpha, beta, gamma.
new_model<-HoltWinters(amts)
NM_pred<-data.frame(predict(new_model,n.ahead = 4))

plot(forecast(new_model,h=4))
#Forecast value for next 4 quarters
forecast_new<-data.frame(forecast(new_model,h=4))
forecast_new
