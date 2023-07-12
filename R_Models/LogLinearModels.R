
Doctors1<- read.csv("C://Users//...//Doctors.csv")
doctors<-data.frame(Doctors1)
head(doctors)

library(MASS)
#### R code (Poisson regression model)
deaths<-doctors$deaths
age<-doctors$age
agesq<-doctors$sgesq
smoking<-doctors$smoking
personyears<-doctors$personyears
res.doc<-glm(deaths~age + agesq + smoking + smoking:age + offset(log(personyears)),family=poisson(),data=doctors)
summary(res.doc)


#### Log-linear model
Tumor1<- read.csv("C://...//Tumor.csv")
melanoma<-data.frame(Tumor1)
head(melanoma)

library(MASS)
#### Log-linear model
frequency<-melanoma$Frequ
tumor<-melanoma$tumor
site<-melanoma$site
site
ressat.melanoma<-glm(frequency~site*tumor,family=poisson(),data=melanoma)
summary(ressat.melanoma)
resadd.melanoma<-glm(frequency~site + tumor,family=poisson(),data=melanoma)
summary(resadd.melanoma)
resmin.melanoma<-glm(frequency~1, family=poisson(),data=melanoma)
summary(resmin.melanoma)



##### COVID-19 daily deaths' in South Africa: June 01, 2020 to 12 September 2020
COVID1<- read.csv("C://...//COVID.csv")
COVID<-data.frame(COVID1)
head(COVID)

##### Fit a Poisson regression model for COVID-19 daily deaths' in South Africa
deaths<-COVID$New_deaths
cases<-COVID$New_cases
res.COVID<-glm(deaths~cases,family=poisson(),data=COVID)
summary(res.COVID)