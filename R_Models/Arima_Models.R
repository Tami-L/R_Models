install.packages("astsa")
library(astsa)
library(forecast)
library(tseries)

Data = read.csv("C:/Users/Documents/ElectricityData.csv",TRUE, ";")
class(Data)
DataTS =ts(Data$Electricity,start =c(2000/01/01),frequency = 12)
plot(DataTS, ylab="Electricity",type="l",xlab="Months")
acf(DataTS)# The data is not stationary
pacf(DataTS)
adf.test(DataTS)#pval>0.05 the data is not stationary

#Convert nonStationary data to Stationary
gdpmodel=auto.arima(DataTS,ic="aic", trace = TRUE)
gdpmodel#
#Check stationarity
acf(ts(gdpmodel$residuals))
pacf((ts(gdpmodel$residuals)))
#Forecast
gdpforecast1 =forecast(gdpmodel,level = c(95),h=1*12)
gdpforecast2 =forecast(gdpmodel,level = c(95),h=2*12)
plot(gdpforecast1)
plot(gdpforecast2)
#Validate
#Insiviadual Residuals
Ljung.Result=Box.test(DataTS$residuals,lag=200,type = "Ljung-Box")
Ljung.Result2=Box.test(gdpmodel$residuals,lag=1,type = "Ljung-Box")
#the pvalue> 0.05, so the data does not have any autocorrelation

#### Model diagnostics
tsdiag(gdpmodel)  

### Residual diagnostics
install.packages("FitAR")
library(FitAR)

res=gdpmodel$residuals





sarima()
ggtsdisplay(DataTS)
BoxCox.lambda(DataTS)#Perfect linear Transformation
y.sdiff =diff(DataTS,lag=12,differences = 1)
ggtsdisplay(y.sdiff)
y.rdiff =diff(DataTS)
ggtsdisplay(y.rdiff)
y.r_sdiff =diff(diff(DataTS,lag=12,differences = 1))
ggtsdisplay(y.r_sdiff)
fitArim= arima(DataTS,order=c(0,1,0), seasonal = c(1,1,0))
autoplot(fitArim)

class(DataTS)

#Plot TS and ACF
par(mfrow=c(2,2))#Display plots as a matrix
plot(DataTS, ylab="Electricity",type="l",xlab="Months")
acf(DataTS,lag.max=48,main=NA)

### Analyse the growth date
gnpgr1<-diff(log(DataTS)) ### Growth rate
gnpgr<-ts(gnpgr1,start=c(2000/01/01),frequency=4)
par(mfrow=c(1,1))
plot(gnpgr, ylab="GNP Growth Rate",type="l",xlab="Time")  
#abline(h=0.01, col="blue")  

#### Model identification
par(mfrow=c(2,1))
acf(gnpgr,24,main=NA)
pacf(gnpgr,24,main=NA)