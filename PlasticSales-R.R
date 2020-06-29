#Load Data
PlasticSales<-read.csv("E:\\R-folder\\01-Assignments\\Forecasting\\PlasticSales.csv")

#Data Summariazation
summary(PlasticSales$Sales)

getmode<-function(x){
  uniq=unique(x)
  uniq[which.max(tabulate(match(x,uniq)))]
}
x<-c(PlasticSales$Sales)
Sales_mode<-getmode(x)
print(Sales_mode)
sd(PlasticSales$Sales)
var(PlasticSales$Sales)
hist(PlasticSales$Sales)

##-____________________________________________________________
plot(PlasticSales$Sales, type = 'l')

#Create dummay variables
X<-data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
colnames(X)<-month.abb #Assigning the month name

PlasticSales["log_Sales"]<-log(PlasticSales["Sales"])
PlasticSalesData<-cbind(PlasticSales,X)
PlasticSalesData["t"]<-1:60
PlasticSalesData["t_Square"]<-PlasticSalesData["t"]*PlasticSalesData["t"]
attach(PlasticSalesData)

#Data Partition
train<-PlasticSalesData[1:48,]
test<-PlasticSalesData[-c(1:48),]

library(Metrics)
##Linear Model_______________________
Ln_model<-lm(Sales~t, data=train)
Ln_pred<-data.frame(predict(Ln_model, interval = "predict", newdata = test))
rmse_Ln<-rmse(test$Sales,Ln_pred$fit)
rmse_Ln

##Exponential Model_________________________
Expo_model<-lm(log_Sales~t, data = train)
Expo_pred<-data.frame(predict(Expo_model, interval = "predict", newdata = test))
rmse_Expo<-rmse(test$Sales, exp(Expo_pred$fit))
rmse_Expo

##Quadratic Model___________________________
Quad_model<-lm(Sales~t+t_Square, data = train)
Quad_pred<-data.frame(predict(Quad_model, interval = "predict", newdata = test))
rmse_Quad<-rmse(test$Sales, Quad_pred$fit)
rmse_Quad

##Additive Seasonality______________________
Add_sea_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
Add_sea_pred<-data.frame(predict(Add_sea_model, interval = "predict", newdata = test))
rmse_Add_sea<-rmse(test$Sales, Add_sea_pred$fit)
rmse_Add_sea

##Additive Seasonality with Linear______________________
Add_sea_ln_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_ln_model)
Add_sea_ln_pred<-data.frame(predict(Add_sea_ln_model, interval = "predict", newdata = test))
rmse_Add_sea_ln<-sqrt(mean((test$Sales-Add_sea_ln_pred$fit)^2,na.rm=T))
rmse_Add_sea_ln

##Additive Seasonality with Quadratic______________________
Add_sea_quad_model<-lm(Sales~t+t_Square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_quad_model)
Add_sea_quad_pred<-data.frame(predict(Add_sea_quad_model, interval = "predict", newdata = test))
rmse_Add_sea_quad<-sqrt(mean((test$Sales-Add_sea_quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_quad

##Multiplicative Seasonality _________________________________
Multi_sea_ln_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Multi_sea_ln_model)
Multi_sea_ln_pred<-data.frame(predict(Multi_sea_ln_model, interval = "predict", newdata = test))
rmse_Multi_sea_ln<-sqrt(mean((test$Sales-exp(Multi_sea_ln_pred$fit))^2,na.rm = T))
rmse_Multi_sea_ln

##Multiplicative Seasonality Linear trend_________________________________
Multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Multi_add_sea_model)
Multi_add_sea_pred<-data.frame(predict(Multi_add_sea_model, interval = "predict", newdata = test))
rmse_Multi_add_sea<-sqrt(mean((test$Sales-exp(Multi_add_sea_pred$fit))^2,na.rm = T))
rmse_Multi_add_sea

#Preaparing table on model and RMSE values
model<-c("rmse_Ln","rmse_Expo","rmse_Quad","rmse_Add_sea","rmse_Add_sea_ln","rmse_Add_sea_quad",
         "rmse_Multi_sea_ln","rmse_Multi_add_sea")
rmse_value<-c(rmse_Ln,rmse_Expo,rmse_Quad,rmse_Add_sea,rmse_Add_sea_ln,rmse_Add_sea_quad,
              rmse_Multi_sea_ln,rmse_Multi_add_sea)
table_RMSE<-data.frame(model,rmse_value)
table_RMSE

# Use entire data : Additive Seasonality with Linear has least RMSE value
new_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = PlasticSalesData)
summary(new_model)

# Getting residuals 
resid<-residuals(new_model)
acf(resid,lag.max = 10)

# Building Autoregressive model on residuals consider lag-1  
k<-arima(resid,order=c(1,0,0))
k
windows();
acf(k$residuals,lag.max = 15)

pred_res<-predict(arima(resid,order = c(1,0,0)),n.ahead = 12)
pred_res
pred_res$pred

##Predicting new data_________
library(readxl)
test.data<-read_excel(file.choose(),1) #Predict_new-Plastic sales.xlsx
pred_new<-data.frame(predict(new_model, interval = "predict", newdata = test.data))
pred_new

pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)

