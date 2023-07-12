library(nnet)
library(MASS)
CARSP<- read.csv("C://Users//....//CarP.csv")
head(CARSP)
CARS<-data.frame(CARSP)
head(CARS)
response<-CARS$Response
age<-CARS$Age
sex<-CARS$Sex
frequency<-CARS$Frequency

###### Fit nominal logistic regression model
res.cars=multinom(response~factor(age)+factor(sex),weights=frequency,data=CARS)
summary(res.cars)

#### Log-likelihood values for the FULL model
logLik((res.cars))


###### Fit Null (minimal) model
res.carsN=multinom(response~1,weights=frequency,data=CARS)
summary(res.carsN)

#### Log-likelihood values for the NULL model
logLik((res.carsN))


library(MASS)
library(e1071)
######## R code (data entry and manipulation) y=c(6,13,18,28,52,53,61,60)
n=c(59,60,62,56,63,59,62,60) x=c(1.6907,1.7242,1.7552,1.7842,1.8113,1.8369,1.8610,1.8839) n_y=n-y
beetle.mat=cbind(y,n_y)
pi=y/n


Fit.GLM.3=glm(beetle.mat~x, family=binomial(link="probit"))
summary(Fit.GLM.3)

plot(residuals(Fit.GLM.1)~fitted(Fit.GLM.1),xlab = expression(hat(mu)[i]), ylab = expression(r[i]))
abline(0, 0, lty = 2)
plot(Fit.GLM.1)






