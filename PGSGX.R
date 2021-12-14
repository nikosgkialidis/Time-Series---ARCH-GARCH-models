#PGSGX FUND
rm(list = ls())
library(tseries)
library(fGarch) 
library(rugarch)
library(Hmisc)
library(readxl)


funds <- read_excel("Assignment-April-2021/funds.xlsx",sheet = "PGSGX", col_types = c("date","numeric", "numeric", "numeric","numeric", "numeric", "numeric","numeric"))
head(funds)

i<-funds$PGSGX
x1<-funds$X1/100
x2<-funds$X2/100
x3<-funds$X3/100
x4<-funds$X4/100
x5<-funds$X5/100
x6<-funds$X6/100
par(mfrow=c(1,1))

# TIME SERIES OBJECTS
y<-ts(i,frequency = 12,start=c(1991,8))
plot(y,type="l", col='blue', lwd=1, main="Time Series plot of the PGSGX fund", ylab="Returs")

x1t<-ts(x1,frequency = 12,start=c(1991,8))
x2t<-ts(x2,frequency = 12,start=c(1991,8))
x3t<-ts(x3,frequency = 12,start=c(1991,8))
x4t<-ts(x4,frequency = 12,start=c(1991,8))
x5t<-ts(x5,frequency = 12,start=c(1991,8))
x6t<-ts(x6,frequency = 12,start=c(1991,8))
par(mfrow=c(2,3))
plot(x1t,type="l", col='red', lwd=1, main="Time Series plot of Mkt-RF", ylab="Mkt-RF")
plot(x2t,type="l", col='red', lwd=1, main="Time Series plot of SMB", ylab="SMB")
plot(x3t,type="l", col='red', lwd=1, main="Time Series plot of HML", ylab="HML")
plot(x4t,type="l", col='red', lwd=1, main="Time Series plot of RMW", ylab="RMW")
plot(x5t,type="l", col='red', lwd=1, main="Time Series plot of CMA", ylab="CMA")
plot(x6t,type="l", col='red', lwd=1, main="Time Series plot of MOM", ylab="MOM")


# IDENTIFICATION STEP
# Create Autocorrelation and partial autocorrelation plots of the stationary series j
par(mfrow=c(1,1))
acf(ts(y,freq=1), 24,xlim=c(1,24),ylim=c(-0.15,0.15), main="ACF of returns")       
pacf(ts(y,freq=1), 24,xlim=c(1,24),ylim=c(-0.15,0.15), main="PACF of returns") 

# AUTOCORELLATION TEST
Box.test(y,24,type="Box-Pierce")
Box.test(y,24,type="Ljung-Box")


# EXPLORE LINEAR RELATIONSHIPS
# Correlation coefficients and p- values
cor(cbind(y,x1,x2,x3,x4,x5,x6))
rcorr(as.matrix(cbind(y,x1,x2,x3,x4,x5,x6))) 
# Scatterplot of all variables
pairs(cbind(y,x1,x2,x3,x4,x5,x6)) 


# MULTIPLE REGRESSION MODEL
MR.all <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 )
summary(MR.all)

# Diagnostic tests for the residuals
# Autocorrelation of the residuals
par(mfrow=c(1,2))
acf(MR.all$residuals, 24,xlim=c(1,24),ylim=c(-0.15,0.15),main="ACF of Multiple Regression Residuals")
pacf(MR.all$residuals, 24,xlim=c(1,24),ylim=c(-0.15,0.15),main="PACF of Multiple Regression Residuals")
Box.test(MR.all$residuals,17,type="Box-Pierce")

# Autocorrelation of the squared residuals
acf(MR.all$residuals^2,24,xlim=c(1,24),ylim=c(-0.5,0.5),main="ACF of Multiple Regression Squared Residuals")
pacf(MR.all$residuals^2, 24,xlim=c(1,24),ylim=c(-0.5,0.5),main="PACF of Multiple Regression Squared Residuals")
Box.test(MR.all$residuals^2,24,type="Box-Pierce")

# Normality test
jarque.bera.test(MR.all$residuals)
shapiro.test(MR.all$residuals) 
par(mfrow=c(1,1))
qqnorm(MR.all$residuals) 
qqline(MR.all$residuals) 

# ARMA(2,2) for regression SQUARED residuals
arma22res=arima(MR.all$residuals^2, order=c(2,0,2),include.mean=FALSE)
# Autocorrelation of the AR(1) residuals
acf(residuals(arma22res),24,xlim=c(1,24),ylim=c(-0.25,0.25),main="ACF of AR(1)")
pacf(residuals(arma22res),24)
Box.test(residuals(arma22res),24,type="Box-Pierce")


# Normality test of the AR(1)
jarque.bera.test(residuals(arma22res))
shapiro.test(residuals(arma22res)) 
qqnorm(residuals(arma22res)) 
qqline(residuals(arma22res)) 


# MULTIPLE REGRESSION MODEL + ARMA + GARCH
X<-matrix(cbind(x1,x2,x3,x4,x5,x6),ncol=6)
spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(2,2) ), 
                   mean.model = list(armaOrder=c(0,0,0), include.mean = F, external.regressors = X),
                   distribution.model = "std")
spec1
modelres <- ugarchfit(spec = spec1, data = y)
modelres

#Better
X<-matrix(cbind(x1,x2,x5,x6),ncol=4)
spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1) ), 
                   mean.model = list(armaOrder=c(1,0,0), include.mean = FALSE, external.regressors = X),
                   distribution.model = "std")
spec2
modelres <- ugarchfit(spec = spec2, data = y)
modelres

post <- read_excel("Assignment-April-2021/funds.xlsx", sheet = "PGSGX-POST", col_types = c("date","numeric","numeric","numeric", "numeric", "numeric","numeric", "numeric"))



x1p<-post$X1/100
x2p<-post$X2/100
x5p<-post$X5/100
x6p<-post$X6/100


pred<-ugarchforecast(modelres,n.ahead = 24 ,external.forecasts = list(mean(x1),mean(x2),mean(x5),mean(x6)))
yhat<-pred@forecast$seriesFor
ypost<-post$PGSGX
MSFE<-sum((yhat-ypost)^2)/24
MSFE
sign<-yhat*ypost
hit<-length(sign[sign>0])/24
hit









