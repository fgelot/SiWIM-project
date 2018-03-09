#### Sourcing prepare data before testing models

setwd("D:/Dropbox/Data science/Formation CEPE/Projet/")
source("Time series SIWIM initialization.R")
source("Time series SIWIM before models.R")


################# Classical models ###################################

#Transform time serie to zoo
x <- zoo(siwim_data_hours$Count,siwim_data_hours$time_step)

autoplot(x)

frequency(x)

gglagplot(x)

plot(rollmean(x,24))

plot(rollmean(x,48))

plot(rollmean(x,168))

#Trasnform time serie to xts

x3 <- xts(siwim_data_hours$Count, order.by = siwim_data_hours$time_step)

x4 <- x3 - lag(x3,k = 18)

x5 <- diff(x3, lag = 24, differences = 1)

plot(x3)
plot(x4)
plot(x5)

x6 <- lag(lag(x3,k = 24), k =7)

plot(x6)

acf(x3, lag.max = 2700)

# Valeur critique ACF
n = length(x3)
1.96/sqrt(n)

res_acf <- acf(x3, lag.max = 168, plot = FALSE)
res2_acf <- cbind.data.frame(lag = res_acf$lag/3600, acf = res_acf$acf)
filtered_acf <- res2_acf[(abs(res2_acf$acf) > 1.96/sqrt(n)),]



pacf(x3, lag.max = 168)
res_pacf <- pacf(x3, lag.max = 168, plot = FALSE)
res2_pacf <- cbind.data.frame(lag = res_pacf$lag/3600, pacf = res_pacf$acf)

res2_pacf

filtered_pacf <- res2_pacf[(res2_pacf$pacf > 1.96/sqrt(n) | res2_pacf$pacf < -1.96/sqrt(n)),]

plot(filtered_acf, type='l')
plot(filtered_pacf, type = 'l')

merge(filtered_acf, filtered_pacf,by = "lag")

acf(x4, na.action = na.pass)

## ets : Exponential smoothing state space model

decomp <- ets(x3)

plot(decomp)

decomp$states[,"s1"]

plot(decomp$residuals)

# Smooting average

x_SMA <- SMA(x3, n = 24)

plot(x_SMA)

# Transform time serie to msts

#x2 <- msts(siwim_data_hours$Count, seasonal.periods = c(8760, 365), start = 2017 + 07/12 + 5/365 + 23/8760)
x2 <- msts(siwim_data_hours$Count, seasonal.periods = c(24, 24*7), start = 2017 + 07/12 + 5/365 + 23/8760)

# double_hw <- dshw(x2,24, 24*7)
# frequency(x2)

x2[2]

plot(x2)

seasonaldecomp <- tbats(x2)
plot(seasonaldecomp)

seasonaldecomp$parameters

accuracy(seasonaldecomp)

forecast(seasonaldecomp, 168)
plot(forecast(seasonaldecomp, 168 *4,level = 0.5))

seasonaldecomp2 <- tbats(siwim_data_hours$Count)
plot(seasonaldecomp2)


accuracy(seasonaldecomp2)

plot(forecast(seasonaldecomp2, 168 *4))


## FOurier decomposition
k <- 2
fuur <- fourier(x2, K = c(k,k))

plot(fuur[,5])

## Holt winters
#Simple exponential smoothing
siwim_data_hw_ses <- HoltWinters(x3 ,beta = FALSE, 
                                 gamma = FALSE)

siwim_data_hw_ses$fitted
siwim_data_hw_ses$x
plot(siwim_data_hw_ses)

predict(siwim_data_hw_ses, x3)

#Exponential smoothing
siwim_data_hw_es <- HoltWinters(x3, gamma = FALSE)

plot(siwim_data_hw_es)

# Forecast holt winters
siwim_data_hw_es.forecast <- forecast(siwim_data_hw_es, h = 1917)
#plot.forecast(siwim_data_hw_es.forecast)

plot(siwim_data_hw_es, predict(siwim_data_hw_es, 1917, prediction.interval = TRUE, level = 0.90) )
siwim_data_hw_es.forecast$fitted
accuracy(siwim_data_hw_es.forecast$fitted)

#Box test sur forecast
Box.test(siwim_data_hw_es.forecast$residuals, lag = 35, type = "Ljung-Box")

# test Dickey Fuller for stationarity detection
X <- siwim_data_hours$Count
lags <- 0
z <- diff(X)
n <- length(z)
z.diff <- embed(z, lags+1)[,1]
z.lag.1 <- X[(lags+1):n]
summary(lm(z.diff~0+z.lag.1 ))


#Augmented Dickey Fuller
lags <- 1
z <- diff(X)
n <- length(z)
z.diff <- embed(z, lags+1)[,1]
z.lag.1 <- X[(lags+1):n]
k <- lags + 1
z.diff.lag <- embed(z, lags+1)[, 2:k]

summary(lm(z.diff~0+z.lag.1+z.diff.lag ))

#Augmented Dickey Fuller with trend and drift
summary(lm(z.diff~1+z.lag.1+z.diff.lag ))

time <- 1:length(z.diff)

#Augmented Dickey Fuller with trend and drift and time trend
summary(lm(z.diff~1+time+z.lag.1+z.diff.lag ))

# with tseries function

#Simple Dickey_fuller test
adf.test(siwim_data_hours$Count, k = 0)

#Augmented Dickey-fuller test

test <- adf.test(siwim_data_hours$Count, k = 2000)
test$p.value
p_top <- 0
lag_top <- 0

for(i in 1:1000){
  print(i)
  test <- adf.test(siwim_data_hours$Count, k = i)
  if(p_top < test$p.value){
    p_top <- test$p.value
    lag_top <- i
  }
}
p_top
lag_top

## no unit root

#Number of differences required for a stationary series
ndiffs(X)

#Autocorrelation Function (ACF): It is a measure of the correlation between the the TS 
# with a lagged version of itself. For instance at lag 5, ACF would compare series at 
#time instant 't1'.'t2' with series at instant 't1-5'.'t2-5' (t1-5 and t2 being end points).

res <- acf(X)
res$acf

#PEriodicity equal to 24
Y <- diff(X,24)
acf(Y,lag=36,lwd=3)
pacf(Y,lag=36,lwd=3)

# Partial Autocorrelation Function (PACF): This measures the correlation between the TS 
# with a lagged version of itself but after eliminating the variations already explained 
# by the intervening comparisons. Eg at lag 5, it will check the correlation but remove 
# the effects already explained by lags 1 to 4.

res2 <- pacf(X)

res2$acf


## ARIMA(p,d, q) :

#Number of AR (Auto-Regressive) terms (p): AR terms are just lags of dependent variable. 
#For instance if p is 5, the predictors for x(t) will be x(t-1)..x(t-5).
#Number of MA (Moving Average) terms (q): MA terms are lagged forecast errors in prediction equation. 
#For instance if q is 5, the predictors for x(t) will be e(t-1)..e(t-5) 
#where e(i) is the difference between the moving average at ith instant and actual value.
#Number of Differences (d): These are the number of nonseasonal differences, 
#i.e. in this case we took the first order difference. 
#So either we can pass that variable and put d=0 or pass the original variable and put d=1. 
#Both will generate same results.


# q - The lag value where the ACF chart crosses the upper confidence interval for the first time
# p - The lag value where the PACF chart crosses the upper confidence interval for the first time. 

# p = 1 and q = 8

# AR model 

model_AR <- arima(X, order = c(1,0,0))

accuracy(model_AR)

forecast(model_AR, 168)
plot(forecast(model_AR, 168))
length(X)

plot(forecast(model_AR, n = 2728, xreg = X))

plot(model_AR$residuals)
acf(model_AR$residuals)
pacf(model_AR$residuals)

# MA model
model_MA <- arima(X, order = c(0,0,8))
accuracy(model_MA)

forecast(model_MA, 168)
plot(forecast(model_MA, 168))

plot(model_MA$residuals)
acf(model_MA$residuals)
pacf(model_MA$residuals)

# Composite model
model_ARMA <- arima(X, order = c(1,0,8))
accuracy(model_ARMA)

forecast(model_ARMA, 168)
autoplot(forecast(model_ARMA, 168))

plot(model_ARMA$residuals)
acf(model_ARMA$residuals)
pacf(model_ARMA$residuals)

# Auto ARIMA model
model_auto_ARMA <- auto.arima(X)

accuracy(model_auto_ARMA)

summary(model_auto_ARMA)

forecast(model_auto_ARMA, 168)
autoplot(forecast(model_auto_ARMA, 168))

plot(model_auto_ARMA$residuals)
acf(model_auto_ARMA$residuals)
pacf(model_auto_ARMA$residuals)

## auto arima with season

model_auto_ARMA_seas <- auto.arima(ts(data=X,frequency = period))

accuracy(model_auto_ARMA_seas)

summary(model_auto_ARMA_seas)
autoplot(forecast(model_auto_ARMA_seas, 168))

## auto arima with double seasons
model_auto_ARMA_double <- auto.arima(x6)

accuracy(model_auto_ARMA_double)

summary(model_auto_ARMA_double)

autoplot(forecast(model_auto_ARMA_double, 168))

# plot(model_auto_ARMA$fitted)
plot(model_auto_ARMA$fitted, type = "l", col = "red")
lines(X, col = "green")

model_auto_ARMA_F <- auto.arima(X,stepwise = F)
acc <- accuracy(model_auto_ARMA_F)
dim(acc)

summary(model_auto_ARMA_F)

autoplot(forecast(model_auto_ARMA_F, 168))

model_auto_ARMA$aicc

model_auto_ARMA_F$aicc

#tsdiag(model_auto_ARMA)

# ets model
model_ets <- ets(X)
accuracy(model_ets)

summary(model_ets)

plot(model_ets$fitted, type = "l", col = "red")
lines(X, col = "green")

pred$pred


#models comparison with tsCV

#to be done

## Multi ARIMA brut force

p_max <- 8
d_max <- 1
q_max <- 15

res_acc <-  matrix(0,nrow = (p_max+1) * (1+d_max) * (q_max+1), ncol = 10) 
# array(0, dim=c(p_max * d_max * q_max,10))

colnames(res_acc) <- c("p", "d", "q", "Mean Error", "Root Mean Squared Error", 
                       "Mean Absolute Error", "Mean Percentage Error", "Mean Absolute Percentage Error",
                       "Mean Absolute Scaled Error", "Autocorrelation of errors at lag 1")
ii <- 1

for (p in 0:p_max) {
  for(d in 0:d_max){
    for(q in 0:q_max){
      model <- arima(X, order = c(p,d,q))
      res_acc[ii,1]<- p
      res_acc[ii,2]<- d
      res_acc[ii,3]<- q
      res_acc[ii,4:10] <- accuracy(model)
      ii <- ii +1
    }
  }
}


min(res_acc[,"Root Mean Squared Error"])
min(res_acc[,5])

write.csv(res_acc, file = "accurracy_multi_ARIMA.csv")
res_acc[100:200,]#[,5]

## optimal ARIMA 6,1,10
model_opt <- arima(X, order = c(6,1,10))
accuracy(model_opt)

forecast(model_MA, 168)
plot(forecast(model_MA, 12))

# Trend detection
T=1:length(X)
plot(T,X,type="l",xlim=c(0,2750))
reg=lm(X~T)
abline(reg,col="red")

Y=residuals(reg)
acf(Y,lag=36,lwd=3)

# SARIMA model
model_SARIMA <- arima(X, order = c(6,1,10), seasonal = list(order = c(0, 1, 2), 
                                                            period=24))
accuracy(model_SARIMA)

plot(forecast(model_SARIMA,168))