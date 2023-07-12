library(MASS)   ### GLM fitting
library(e1071)
Carbo<- read.csv("C://Users//...//Carbohydrate.csv")
CarbohydrateD=data.frame(Carbo)  # to change HH into data frame
CarbohydrateD
head(CarbohydrateD)
tail(CarbohydrateD)
carbohydrate<-CarbohydrateD$Carbohydrate
age<-CarbohydrateD$Age
weight<-CarbohydrateD$Weight
protein<-CarbohydrateD$Protein

###### Fit multiple linear regression model
##### For Model (6)
########### R code (linear model)##############
res.lm=lm(carbohydrate~age+weight+protein,data=CarbohydrateD)
summary(res.lm)

####### For Model (7)
res.lm1=lm(carbohydrate~weight+protein,data=CarbohydrateD)
summary(res.lm1)

####### Construct the ANOVA table for Model (6) and Model (7) as follows:
anova(res.lm, res.lm1)

############ Fit generalized linear model
########### R code (generalized linear model)
res.glm=glm(carbohydrate~age+weight+protein,family=gaussian,data=CarbohydrateD)
summary(res.glm)

####### GLM for Model (7)
res.glm1=glm(carbohydrate~weight+protein,family=gaussian,data=CarbohydrateD)
summary(res.glm1)
BIC(res.glm1)  ### Bayesian information criterion
AIC(res.glm1)  ### Akaike information criterion


######## Cross Validation
###### The following code estimates the cross-validated prediction error for the
######### carbohydrate diet example in R using the "cvTools" library (Alfons 2012).
############### R code (cross-validated prediction error)
####install.packages("cvTools")
library(cvTools)
res.lm=lm(carbohydrate~age+weight+protein,data=CarbohydrateD)
cvFit(res.lm, data=CarbohydrateD,K=5, R=10,y=carbohydrate)


########### R code (stepwise model selection)
##### install.packages("olsrr")
library(olsrr)
res.lm=lm(carbohydrate~age+weight+protein,data=CarbohydrateD)
ols_step_both_p(res.lm, details=TRUE)

