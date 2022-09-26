library(forecast)
library(astsa)
setwd("E:\\Ha!l!e\\R bianchengke\\rdata")
data<-read.csv("stocks.csv")
plot.ts(data$Date,data$Price,type="l")
plot(log(data$Price),type="l",xlab="time",ylab="log(x)",main="log of stock price")
plot(1:1194,sqrt(data$Price),type="l",xlab="time",ylab="sqrt(x)",main="sqrt of stock price")
lines(log(data$Price),col=3)
head(data)
tail(data)


plot(diff(log(data$Price)),type="l",xlab="Trading Day",ylab="",main="First Order Difference of Residuals( Log(Price) )")
plot(diff(data$Price),type="l",xlab="Trading Day",ylab="",main="First Order Difference of Residuals( Price )")
plot(diff(log(data$Price)),type="l",main ="First Order Difference of Residuals", xlab = "Trading Day",ylab="")
data1=log(data$Price)

plot(diff(diff(log(data$Price)),lag=20),type="l")
auto.arima(data1)

plot(diff(data1),type="l")
acf2(diff(data1),main="ACF and PACF of the Residuals")
acf2(diff(diff(data1),lag=255))


model1 = sarima(data1,p=0,d=1,q=10,S=23,P=0,D=1,Q=1)
model2 = sarima(data1,p=20,d=1,q=0,S=0,P=0,D=0,Q=0)
model3 = sarima(data1,p=0,d=1,q=6,S=22,P=0,D=1,Q=1)
model4 = sarima(data1,p=0,d=1,q=3,S=20,P=1,D=1,Q=2)

start_year <- 835
end_year <- 1194
sum_squared_errors <- c(model1=0, model2=0, model3=0, model4=0)
sum_squared_errors =matrix(NA,nrow=10,ncol=4)
for (year in start_year:end_year){
  train_set <- window(data1, end=year-0.001)
  test_set <- window(data1, start=year, end=year + 1 - 0.01)
  #
  forecast1 <- sarima.for(train_set, n.ahead=12, p=4,d=1,q=1,S=12,P=0,D=1,Q=1)$pred
  forecast2 <- sarima.for(train_set, n.ahead=12, p=0,d=1,q=4,S=12,P=0,D=1,Q=1)$pred
  forecast3 <- sarima.for(train_set, n.ahead=12, p=1,d=1,q=3,S=12,P=0,D=1,Q=1)$pred
  #
  model4 <- lm(train_set ~ poly(1:length(train_set), degree=3, raw=TRUE) + seasonaldummy(train_set))
  test_matrix <- model.matrix( ~ poly((length(train_set) + 1):(length(train_set) + 12), degree=3,
                                      raw=TRUE) + seasonaldummy(test_set) )
  forecast4 <- test_matrix %*% model4$coefficients
  #
  sum_squared_errors[year-1984,1] = sum((forecast1 - test_set)^2)
  sum_squared_errors[year-1984,2] = sum((forecast2 - test_set)^2)
  sum_squared_errors[year-1984,3] = sum((forecast3 - test_set)^2)
  sum_squared_errors[year-1984,4] = sum((forecast4 - test_set)^2)
}
apply(sum_squared_errors,2,mean)



length(data1)
t=1:1194
1194*0.7
lm1<-lm(dataf$data1~t+dataf$month)
lm1$residuals
plot(lm1$residuals,type='l')
data2=diff(lm1$residuals)
plot(diff(lm1$residuals),type='l')
acf2(data2)
auto.arima(data2)
sarima(data2,p=0,d=0,q=6,P=0,D=0,Q=0,S=0)

dataf<- data.frame(data1)
dataf
dataf$month<-month(data$Date)
dataf
dataf$month<-as.factor(dataf$month)




library(lubridate)
month(data$Date)
data[4]<-month(data$Date)
data
 

setwd("E:\\Ha!l!e\\R bianchengke\\rdata")
load("gas_data.Rdata")
logdata=log(gas_data)
head(logdata)
logdata
sarima.for(logdata,n.ahead = 12,p=4,d=1,q=1,S=12,P=0,D=1,Q=1)
#https://edwinth.github.io/blog/padr-intro/


start_day <- 954
end_day <- 1194
dat<-stocks[,c(3,4,8,9)]

#set the vector sum_squared_errors
sum_squared_errors <- c(ds_model1=0, ds_model2=0, yf_model1=0, yj_model1=0,lm_model1=0,lm_model2=0,lm_model3=0)


#calculate mse for two lm models using the same split as others
for (day in seq(start_day,end_day,by=10)) {
  # spilt our data
  train_order<-window(1:1194, end=day-0.001)
  test_order<-window(1:1194,start=day,end=day+10-0.01)
  train_set<-dat[train_order,]
  test_set<-dat[test_order,]
  model7<-lm(data=train_set,Price~time+I(time^2)+month+weekday)
  arima<-arima(model7$residuals,order=c(2,0,0))
  forecast7_1<-predict(model7,test_set,n.ahead=10)
  forecast7_2<-predict(arima,n.ahead=10)
  forecast7<-forecast7_1+forecast7_2$pred
  sum_squared_errors[7] = sum_squared_errors[7] + sum((forecast7-test_set$Price)^2)}
sum_squared_errors/(end_day - start_day + 1)