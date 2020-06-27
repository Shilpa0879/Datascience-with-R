library(readxl)

#Load Data
Airlines<-read_xlsx("E:\\R-folder\\01-Assignments\\Forecasting\\Airlines+Data.xlsx")

#Data Summariazation
summary(Airlines$Passengers)

getmode<-function(x){
  uniq=unique(x)
  uniq[which.max(tabulate(match(x,uniq)))]
}
x<-c(Airlines$Passengers)
Psngr_mode<-getmode(x)
print(Psngr_mode)
sd(Airlines$Passengers)
var(Airlines$Passengers)
hist(Airlines$Passengers)

##-____________________________________________________________

plot(Airlines$Passengers,type = "l")

#Creating 11 dummay variables
X<-data.frame(outer(rep(month.abb,length=96),month.abb,"==")+0)
colnames(X)<-month.abb #Assigning month name

AirlineData<-cbind(Airlines,X)
AirlineData["t"]<-1:96
AirlineData["log_Passengers"]<-log(AirlineData["Passengers"])
AirlineData["t_square"]<-AirlineData["t"]*AirlineData["t"]
attach(AirlineData)

#Data Partition
train<-AirlineData[1:84,]
test<-AirlineData[-c(1:84),]

library(Metrics)
##Linear Model___________________________
linear_model<-lm(Passengers~t, train)
linear_pred<-data.frame(predict(linear_model, interval = "predict", newdata = test))
rmse_linear<-rmse(test$Passengers, linear_pred$fit)
rmse_linear

##Exponential Model___________________________
expo_model<-lm(log_Passengers~t, train)
expo_pred<-data.frame(predict(expo_model,interval = "predict", newdata = test))
rmse_expo<-rmse(test$Passengers,exp(expo_pred$fit))
rmse_expo

##Quadratic Model___________________________
Quad_model<-lm(Passengers~t+t_square, data = train)
Quad_pred<-data.frame(predict(Quad_model, interval = 'predict', newdata = test))
rmse_Quad<-rmse(test$Passengers,Quad_pred$fit)
rmse_Quad


##Additive Seasonality Model___________________________
Sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
Sea_add_pred<-data.frame(predict(Sea_add_model,interval = "predict", newdata = test))
rmse_Sea_add<-sqrt(mean((test$Passengers-Sea_add_pred$fit)^2,na.rm=T))
rmse_Sea_add

##Additive Seasonality Linear Model___________________________
Add_Sea_ln_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_Sea_ln_model)
Add_Sea_ln_pred<-data.frame(predict(Add_Sea_ln_model,interval = "predict", newdata = test))
rmse_Add_Sea_ln<-sqrt(mean((test$Passengers-Add_Sea_ln_pred$fit)^2,na.rm=T))
rmse_Add_Sea_ln

##Additive Seasonality with Quadratic ___________________________
Add_Sea_quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_Sea_quad_model)
Add_Sea_quad_pred<-data.frame(predict(Add_Sea_quad_model, interval = "predict", newdata = test))
rmse_Add_Sea_quad<-sqrt(mean((test$Passengers-Add_Sea_quad_pred$fit)^2,na.rm=T))
rmse_Add_Sea_quad

##Multiplicative Seasonality___________________________
multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model, interval = "predict", newdata = test))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm=T))
rmse_multi_sea

##Multiplicative Seasonality Linear trend ___________________________
multi_sea_ln_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_ln_model)
multi_sea_ln_pred<-data.frame(predict(multi_sea_ln_model, interval = "predict", newdata = test))
rmse_multi_sea_ln<-sqrt(mean((test$Passengers-exp(multi_sea_ln_pred$fit))^2,na.rm=T))
rmse_multi_sea_ln

#Preaparing table on model and RMSE values
model<-c("rmse_linear","rmse_expo","rmse_Quad","rmse_Sea_add","rmse_Add_Sea_ln",
         "rmse_Add_Sea_quad","rmse_multi_sea","rmse_multi_sea_ln")
RMSE<-c(rmse_linear,rmse_expo,rmse_Quad,rmse_Sea_add,rmse_Add_Sea_ln,
        rmse_Add_Sea_quad,rmse_multi_sea,rmse_multi_sea_ln)
table_RMSE<-data.frame(model,RMSE)
table_RMSE

#Use entire data : as Multiplicative Seasonality Linear trend has lowest RMSE value
new_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = AirlineData)
summary(new_model)

# Getting residuals 
resid<-residuals(new_model)
acf(resid,lag.max = 10)

# Building Autoregressive model on residuals consider lag-1  
k<-arima(resid,order = c(1,0,0))
k
windows()
acf(k$residuals,lag.max = 15)

pred_res<-predict(arima(resid,order = c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred

####################### Predicting new data ############################# 
library(readxl)
test.data<-read_excel(file.choose(),1) #Predict_new.xlsx

pred_new<-data.frame(predict(new_model, interval = "predict", newdata = test.data))
pred_new

pred_new$fit<-pred_new$fit+pred_res$pred
View(pred_new)

pred_new['fit']<-as.data.frame(pred_new$fit+pred_res$pred)
lwr<-as.data.frame(pred_new$lwr)
upr<-as.data.frame(pred_new$upr)
data<-cbind.data.frame("fit"=pred_new$fit,"lwr"=lwr$'pred_new$lwr',
                       "upr"=upr$'pred_new$upr')
