library(readr)
library(ggplot2)
library(TSA)
library(dplyr)
library(forecast)


# reading data 
airport <- read.csv("Air_Traffic_Passenger_Statistics.csv")

# Grouping the data on the monthly basis
airport$Activity.Period.Start.Date <- as.Date(airport$Activity.Period.Start.Date)
airport_grouped <- airport %>% 
  group_by(Activity.Period.Start.Date) %>%
  summarise(total_passenger_count = sum(Passenger.Count, na.rm = TRUE))

plot(y = airport_grouped$total_passenger_count,x = airport_grouped$Activity.Period.Start.Date,
     type = "o",ylab = "Values",xlab = "time",
     main = "Plot of Total Passenger count vs Time")

# extracting pre covid data
precovid <- subset(airport_grouped, Activity.Period.Start.Date <= as.Date("2019-12-31"))
postcovid <- subset(airport_grouped, Activity.Period.Start.Date > as.Date("2019-12-31"))

plot(y = precovid$total_passenger_count,x = precovid$Activity.Period.Start.Date,
     type = "o",ylab = "Values",xlab = "time",
     main = "Plot of Total Passenger count vs Time")

##### Checking for white noise ####

mean(precovid$total_passenger_count)
#Predicted residuals
pred_e_precovid=precovid$total_passenger_count-mean(precovid$total_passenger_count)  
#standardized predicted residuals
std_pred_e_precovid=pred_e_precovid/sd(pred_e_precovid)  
plot(std_pred_e_precovid,type="o",ylab="Values",xlab="time")
plot(y=std_pred_e_precovid,x=zlag(std_pred_e_precovid,1),ylab="Values at t",xlab="Values at t-1")
hist(std_pred_e_precovid,main="Histogram of predicted residuals",xlab="Predicted Residuals",breaks=10)
qqnorm(std_pred_e_precovid);qqline(std_pred_e_precovid,col="red")
acf(std_pred_e_precovid)

# As can be seen from the graphs, there is a clear upward trend in the residual graph and all
# so we reject white noise assumption and move forward with the model building

# turning it into time series
prets = ts(precovid$total_passenger_count, start=c(1999, 7), end=c(2019, 12), frequency=12)
plot(prets)

post_ts = ts(postcovid$total_passenger_count, start=c(2020,1), end=c(2023,9), frequency=12)
plot(post_ts)
##### Linear Model #####
# fitting the model. 
model1=lm(prets~time(prets))
summary(model1)
# plot residual over time.
plot(prets,type='o',main="Series and fitted linear trend",ylab='Values')
# linear trend line
abline(model1,col="red",lwd=5)

# plot pf the residuals against the fitted trend. 
# white noise residuals would show no pattern associated with the fitted trend.
plot(y=rstudent(model1),x=as.vector(time(prets)),xlab='Time',
     ylab='Standardized Residuals',main="Residuals for linear trend model",type='o')
# plot of standardized residuals against the fitted trend.
plot(y=rstudent(model1),x=fitted(model1),ylab='Standardized Residuals',
     xlab='Fitted Trend Values',type='p')

#checking for normality - is normal if in bell shape.
hist(rstudent(model1),main="Histogram of predicted residuals",xlab="Predicted Residuals",breaks=10)

# qq plot also for normality.
qqnorm(rstudent(model1));qqline(rstudent(model1),col="red")

# correlograms - checking for autocorrelation function
acf(rstudent(model1),main="Sample autocorrelation of predicted residuals")

acf(rstudent(model1),plot=FALSE)    #Print r_k for predicted residuals
acf(prets,plot=FALSE)            #Print r_k for series (check: different from above)
sd(residuals(model1))
varbeta1=function(g0,n,rk){
  rk=c(rk,rep(0,n))   # adding extra 0s in autocorrelation rk (not using values after a chosen lag)
  factor_vbeta1=0
  for(s in 2:n){
    for(t in 1:(s-1)){
      factor_vbeta1=factor_vbeta1+(t-.5*(n+1))*(s-.5*(n+1))*rk[s-t]
    }
  }
  factor_vbeta1=1+(24/(n*(n^2)-1))*factor_vbeta1
  vbeta1<-factor_vbeta1*12*g0/(n*(n^2)-1)
  print(vbeta1)
}
# next line needs to be updated if working with a different series
varbeta1(541131.4^2,246,c( 0.783,0.605, 0.405, 0.215, 0.088))   # Using first 5 lags

####### Quadratic model ##########
t = time(prets)
t2 = t^2
model2 = lm(prets~t+t2)
summary(model2)

# plot residual over time.
plot(ts(fitted(model2)), ylim=c(min(c(fitted(model2), as.vector(prets))), max(c(fitted(model2), as.vector(prets)))), ylab='y', main = "Fitted Quadratic Model", col="red", lwd=5)
lines(as.vector(prets),type="o")
# plot pf the residuals against the fitted trend. 
# white noise residuals would show no pattern associated with the fitted trend.
plot(y=rstudent(model2),x=as.vector(time(prets)),xlab='Time',
     ylab='Standardized Residuals',main="Residuals for linear trend model")
abline(h=0)

# plot of standardized residuals against the fitted trend.
plot(y=rstudent(model2),x=fitted(model2),ylab='Standardized Residuals',
     xlab='Fitted Trend Values',type='p')

#checking for normality - is normal if in bell shape.
hist(rstudent(model2),main="Histogram of predicted residuals",xlab="Predicted Residuals",breaks=10)

# qq plot also for normality.
qqnorm(rstudent(model2));qqline(rstudent(model2),col="red")

# correlograms - checking for autocorrelation function
acf(rstudent(model2),main="Sample autocorrelation of predicted residuals")

acf(rstudent(model2),plot=FALSE)    #Print r_k for predicted residuals
acf(prets,plot=FALSE)            #Print r_k for series (check: different from above)
sd(residuals(model2)) 

# Detrending the time series - linear
detrend_tslm = prets - predict(model1)
plot(detrend_tslm)
acf(detrend_tslm)  
acf(detrend_tslm,lag.max=10,plot=FALSE)
pacf(detrend_tslm)  
eacf(detrend_tslm) 

# trying AR(1)
AR <- arima(detrend_tslm, order = c(1,0,0))
print(AR)
ts.plot(detrend_tslm)
AR_fit <- detrend_tslm- residuals(AR)

# Residual Analysis - Plot of residuals against time
plot(residuals(AR), type = "l", xlab = "Time", ylab = "Residuals",
     main = "Residuals against Time")
abline(h=0, col="red")

# Residual Analysis - Plot of residuals against fitted values
plot(AR_fit, residuals(AR), xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals against Fitted Values")

# Residual Analysis - Histogram
hist(residuals(AR), breaks = 20, main = "Histogram of Residuals",
     xlab = "Residuals", ylab = "Frequency")

# Residual Analysis - Q-Q plot
qqnorm(residuals(AR), main = "Q-Q Plot of Residuals")
qqline(residuals(AR), col = "red")

# Residual Analysis - Correlogram (ACF and PACF)
acf(residuals(AR), main = "ACF of Residuals")
pacf(residuals(AR), main = "PACF of Residuals")
acf(residuals(AR),plot=FALSE)    #Print r_k for predicted residuals
acf(prets,plot=FALSE)            #Print r_k for series (check: different from above)
eacf(prets) 


# trying AR(2)
AR2 <- arima(detrend_tslm, order = c(2,0,0))

print(AR2)
ts.plot(detrend_tslm)
AR_fit <- detrend_tslm- residuals(AR2)

# Residual Analysis - Plot of residuals against time
plot(residuals(AR), type = "l", xlab = "Time", ylab = "Residuals",
     main = "Residuals against Time")
abline(h=0, col="red")

# Residual Analysis - Plot of residuals against fitted values
plot(AR_fit, residuals(AR), xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals against Fitted Values")

# Residual Analysis - Histogram
hist(residuals(AR), breaks = 20, main = "Histogram of Residuals",
     xlab = "Residuals", ylab = "Frequency")

# Residual Analysis - Q-Q plot
qqnorm(residuals(AR), main = "Q-Q Plot of Residuals")
qqline(residuals(AR), col = "red")

# Residual Analysis - Correlogram (ACF and PACF)
acf(residuals(AR), main = "ACF of Residuals")
pacf(residuals(AR), main = "PACF of Residuals")
acf(residuals(AR),plot=FALSE)    #Print r_k for predicted residuals
acf(prets,plot=FALSE)            #Print r_k for series (check: different from above)

# Trying seasonal model
### SARIMA(1,0,1)(0,1,1)12
##Chosen model
sarima_model <- arima(prets,order=c(1,0,1),seasonal=list(order=c(0,1,1), period=12))
summary(sarima_model)
sarima_residuals <- residuals(sarima_model)
acf(sarima_residuals, main = "ACF of SARIMA Residuals")
pacf(sarima_residuals, main = "PACF of SARIMA Residuals")
eacf(sarima_residuals) 
plot(sarima_residuals, type = "l", xlab = "Time", ylab = "Residuals",
     main = "Residuals against Time")
abline(h=0, col="red")

# Residual Analysis - Histogram
hist(sarima_residuals, breaks = 20, main = "Histogram of Residuals",
     xlab = "Residuals", ylab = "Frequency")

# Residual Analysis - Q-Q plot
qqnorm(sarima_residuals, main = "Q-Q Plot of Residuals")
qqline(sarima_residuals, col = "red")

fore2 <- predict(sarima_model, n.ahead=45)
# UI- Upper prediction interval and #LI- lower prediction interval
UI <- fore2$pred + 2 * fore$se  # se: standard error (quantile is 2 as mean=0)
LI <- fore2$pred - 2 * fore$se
ts.plot(prets, fore2$pred, UI, LI, post_ts, col = c("black", "blue", "lightblue", "lightblue", "green"), lty = c(1, 1, 2, 2,1), lwd = 2)
legend("topleft", c("Pre covid", "Forecast", "Error Bounds (95% prediction interval)", "Post Covid"), 
       col = c("black", "blue", "lightblue", "green"), lty = c(1, 1, 2,1))
