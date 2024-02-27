# EXERCISE 6:  Time series analysis in R
library(foreign)
library(stats)
library(splines)
library(mgcv)

nycpm<-read.csv("yourpath/nyc_pmwea9906.csv")

names(nycpm)
dim(nycpm)

#  Plot the original data. 

plot(nycpm$tot)
plot(nycpm$TEMP)
plot(nycpm$dewp)

cor(nycpm$TEMP,nycpm$dewp,use="complete.obs")
cor(nycpm$TEMP,nycpm$RH,use="complete.obs")

# Create lags by shifting one observation down. The function lag is not working properly.
#### library(quantmod) use Lag to create lags

# Also you can use this function:
#Function to take lag of a variable

lagm<-function(x,n) {
  nn<-length(x)
  xn<-c(rep(NA,n),x[1:(nn-n)])
  return(xn)
}

## but we have already lags created in the dataset!

cbind(nycpm$pm25[182:200],nycpm$pm251[182:200],nycpm$pm252[182:200],nycpm$pm253[182:200])

	
# We can check if the mean is equal to the variance as should be in a Poisson distribution

hist(nycpm$tot)
	summary(nycpm$tot)
	mean(nycpm$tot)
	var(nycpm$tot)


## becasue the variance is not equal to the mean we will use a quasi poisson distribution

# Trend and season
## what does it mean to fit a long term trend. For example we can fit 
# a spline with 3 df, plot it and then plot the residuals
# from the plot of the model you can see an almost linear line descreasing with time
# when we plot the residuals you can see that there is still seasonality but the long term 
# decline is gone

### the variable date when imported from csv files is not seen as a number.
### we will create a numeric var that goes from 1 to N and use this variable instead
dim(nycpm)

time<-1:dim(nycpm)[1]

mod1 <- gam(tot ~ s(time,k=4,fx=TRUE), family=quasipoisson, data=nycpm, na.action=na.omit)
  summary(mod1)


  plot(nycpm$tot)
  plot(mod1,scale=0)
  plot(resid(mod1))

### what if we add the other covariates
### is temperature linear?
### which other cov? dow


mod1 <- gam(tot ~ s(time,k=4,fx=TRUE)+s(TEMP,k=4,fx=TRUE)+as.factor(dow), family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod1)

plot(mod1,scale=0)
plot(resid(mod1))

##### add RH and see if the scale par which represent the overdispersion dicrease
mod1 <- gam(tot ~ s(time,k=4,fx=TRUE)+s(TEMP,k=4,fx=TRUE)+s(RH,k=4,fx=TRUE)+as.factor(dow), family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod1)

plot(mod1,scale=0)
plot(resid(mod1))

###  I can use a penalized spline with more df to take into account both the long term trend and seasonality

# 	Examine a Poisson model for total daily mortality with  penalized spline for continuos variables. 
# Use GCV first and then we can use the sp option. 
# Because the mean is not equal to the variance, 
# we can use the quasi-likelihood for Poisson, that is family=quasipoisson.
names(nyc)

mod1 <- gam(tot ~ s(time,bs = "cr",fx=FALSE)+s(TEMP,bs = "cr",fx=FALSE)+as.factor(dow)+ s(RH,bs = "cr",fx=FALSE), family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod1)

  plot(mod1,scale=0)



#### why we did get df=7.4?? because we left the default for k=10
## how many years of data?

length(table(nycpm$yr))  # length(unique(nycpm$yr))

## I will change k to be larger than numbers of years*5 
### for example use 10 knots per year



mod1 <- gam(tot ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+as.factor(dow)+ s(RH,bs = "cr",fx=FALSE), family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod1)

plot(mod1,scale=0)

mod1 <- gam(tot ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+as.factor(dow)+RH, family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod1)

# The above model is not good, GCV estimated too many df for time and temp. 
# Which could be an adequate number of DF for weather and season? 
# First as you can see from the plots there are too many bumps (small) that might have no sense. 
# From previous analysis it seems that for season around 4 to 6 df per year control reasonably for season,
# while for weather variables a total of 3 to 5 df totAL should be OK.
# We can use the sp option to choose an adequate number of df for weather and season.
## there are 8 years of data, therefore we want to estimate around 40 df for seasonality

mod1$sp

mod1 <- gam(tot ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+as.factor(dow)+RH, family=quasipoisson, data=nycpm, na.action=na.omit, sp=c(80,100))
summary(mod1)

mod1 <- gam(tot ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+as.factor(dow)+RH, family=quasipoisson, data=nycpm, na.action=na.omit, sp=c(800,1000))
summary(mod1)

  plot(mod1,scale=0)


## include pollution at lag 0 
mod2 <- gam(tot ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+as.factor(dow)+RH+pm25, family=quasipoisson, data=nycpm, na.action=na.omit, sp=c(800,1000))
summary(mod2)

### the number of obs went down to 2663 because of missing values in PM2.5
### the scale parameter went down

## compute the Relative risk (and 95% CI) in mortality for 10 mg/m3 increase in PM25
beta <- mod2$coef[9]
se   <- sqrt(mod2$Vp[9,9])

exp(beta*10)
exp((beta -1.96*se)*10)
exp((beta +1.96*se)*10)


## distributed lag model

mod2 <- gam(tot ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+as.factor(dow)+RH+ pm25+pm251+pm252+pm253+pm254+pm255,family=quasipoisson, data=nycpm, na.action=na.omit, sp=c(800,1000))
summary(mod2)

## plot of each lag
### a simple plot does not help much
plot(c(mod2$coef[9],mod2$coef[10],mod2$coef[11],mod2$coef[12],mod2$coef[13],mod2$coef[14]))

## we can use the plotCI function
install.packages("gplots")
library(gplots)

## create vectors of coef for plotxlim=c(-0.004,0.004)

lags <- c(0,1,2,3,4,5)

beta <- c(mod2$coef[9],mod2$coef[10],mod2$coef[11],mod2$coef[12],mod2$coef[13],mod2$coef[14])

plotCI(y=beta,x=lags,uiw=1.96*sqrt(diag(mod2$Vp[9:14,9:14])),err="y")
abline(h=0, col="grey")


# plotCI(x=beta,y=lags,uiw=1.96*sqrt(sum(mod2$Vp[9:14,9:14])),err="x")
# abline(v=0, col="grey")

# When you fit a distributed lag model you are interested not only in the effect at each lag, 
# but also in the sum of the effects at each lag.
# 
# sum<-mod2b$coef[8]+mod2b$coef[9]+mod2b$coef[10]+mod2b$coef[11]+mod2b$coef[12]+mod2b$coef[13]
# sesum<-sqrt(sum(mod2b$Vp[8:13,8:13]))

bsum  <- mod2$coef[9]+mod2$coef[10]+mod2$coef[11]+mod2$coef[12]+mod2$coef[13]+mod2$coef[14]
sesum <- sqrt(sum(mod2$Vp[9:14,9:14]))

exp(bsum*10)
exp((bsum -1.96*sesum)*10)
exp((bsum +1.96*sesum)*10)



# Look at the dose-response of PM25

mod2 <- gam(tot ~ s(time,bs = "cr",fx=FALSE,k=60)+s(TEMP,bs = "cr",fx=FALSE)+as.factor(dow)+RH+s(pm25), family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod2)

mod2 <- gam(tot ~ s(time,fx=TRUE,k=40)+s(TEMP,fx=TRUE,k=5)+as.factor(dow)+RH+s(pm25,fx=TRUE,k=4), family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod2)


  plot( mod2, scale=0)

# 	Now we can fit a model with a 2-D smoothing. pers=True gives you the tri-dimensional plot.


mod2 <- gam(tot ~ s(time,fx=TRUE,k=40)+s(TEMP,RH)+as.factor(dow)+pm25,family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod2)

mod2 <- gam(tot ~ s(time,fx=TRUE,k=41)+s(TEMP,pm25)+as.factor(dow)+RH,family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod2)

  plot( mod2, pers=TRUE,select=2) #view second term
	plot( mod2, pers=TRUE,select=2,theta=10,phi=10)
	plot( mod2, pers=TRUE,select=2,theta=50,phi=10)
	plot( mod2, pers=TRUE,select=2,theta=90,phi=10)
	plot( mod2, pers=TRUE,select=2,theta=50,phi=50)


#note, we cannot change the df in a double smoothing term



#########################################################################
################DO BY YOURSELF###########################################

# 1.  do a time series analysis of the effects of temperature and air pollution on CVD mortality in NYC

### Plot the histogram, look at the  mean and variance: which distribution will you use?
hist(nycpm$cvd)
summary(nycpm$cvd)
mean(nycpm$cvd)
var(nycpm$cvd)

## 2. plot the original data for CVD mortality, pollution and the weather variables. 
### The data set is very long.
# then plot the same variables for the first 1500 obs
	par(mfrow=c(2,1))
    plot(nycpm$cvd)
	plot(nycpm$pm25)
	plot(nycpm$TEMP)
	plot(nycpm$RH)

	plot(nycpm$cvd[1:1500])

# 3.Trend and season, Weather and day of week
### include in the model all potential confounders
# Use a penalized spline for all continuous variables 
#  first use GCV to choose the df. 
# to estimate a penalized spline we need to have a large number of knots. 
# use 10 knots per year for seasonality, while we can use the default of 10 knots for weather variables. 
## Do not include PM

mod1 <- gam(cvd ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+s(RH,bs = "cr",fx=FALSE)+ as.factor(dow), family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod1)

plot(mod1,scale=0)


# 4. In	the above model, did GCV estimate too many df? Which could be an adequate number of DF for
# weather and season? First as you can see from the plots there are too many bumps (small) that might 
# have no sense. what about RH?
# From previous published work it seems that for season around 4 to 6 df per year control reasonably 
# for seasonality, while for weather variables a cvd of 3 to 5 df cvd should be OK.
#  use the sp option to choose an adequate number of df for weather and season.

  mod1$sp
mod1 <- gam(cvd ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+RH+ as.factor(dow), family=quasipoisson, data=nycpm, na.action=na.omit,sp=c(400,1000))
summary(mod1)

plot(mod1,scale=0)


# 5.	Now include the pollution at lag 0 in the model and estimate the effect

mod1 <- gam(cvd ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+RH+ as.factor(dow)+pm25, family=quasipoisson, data=nycpm, na.action=na.omit,sp=c(400,1000))
summary(mod1)

plot(mod1,scale=0))

## 6. compute the Relative risk (and 95% CI) in mortality for 10 mg/m3 increase in TSP
          beta <- mod1$coef[9]
          se   <- sqrt(mod1$Vp[9,9])
          
          exp(beta*10)
          exp((beta +1.96*se)*10)
          exp((beta -1.96*se)*10)
          
          print(paste("RR (95%CI): ", round(exp(beta*10),3), " (", round(exp((beta-1.96*se)*10),3), ", ", round(exp((beta+1.96*se)*10), 3), ")" , sep=""))

# 7.	Run the same model with natural spline in GLM: is the result for PM25 similar?

        mod5 <- glm(cvd ~ ns(time,df=41)+ ns(TEMP,df=4)+RH+ as.factor(dow)+pm25,family=quasipoisson, data=nycpm, na.action=na.omit)
        summary(mod5)

# 8.	Also the effect of air pollution could be not at lag 0 but in the previous days. 
# Which lags are more important? Fit the GAM models first with pm25 at lag 1, then lag 2 and then lag 3

mod1 <- gam(cvd ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+RH+as.factor(dow)+pm251, family=quasipoisson, data=nycpm, na.action=na.omit,sp=c(400,1000))
summary(mod1)
        
        
mod1 <- gam(cvd ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+RH+ as.factor(dow)+pm252, family=quasipoisson, data=nycpm, na.action=na.omit,sp=c(400,1000))
summary(mod1)
        
        
mod1 <- gam(cvd ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+RH+ as.factor(dow)+pm253, family=quasipoisson, data=nycpm, na.action=na.omit,sp=c(400,1000))
summary(mod1)



# 9.	fit a distributed lag model, that is a model where we include Pm25 at lag 0,1,2 and lag 3 
# simultaneously in the model

mod1 <- gam(cvd ~ s(time,bs = "cr",fx=FALSE,k=80)+s(TEMP,bs = "cr",fx=FALSE)+RH+as.factor(dow)+pm25+pm251+pm252+pm253, family=quasipoisson, data=nycpm, na.action=na.omit,sp=c(400,1000))
summary(mod1)

##10. plot each lag using the plotCI function


        lags <- c(0,1,2,3)
        
        beta <- c(mod1$coef[9],mod1$coef[10],mod1$coef[11],mod1$coef[12])


plotCI(y=beta,x=lags,uiw=1.96*sqrt(diag(mod1$Vp[9:12,9:12])),err="y")
abline(h=0, col="grey")
        
      

# 11. When you fit a distributed lag model you are interested not only in the effect at each lag, 
# but also in the sum of the effects at each lag.
## compute the sum of the lags and its standard error and 
# then compute the RR and 95% CI of the sum 

sumb <- mod1$coef[9]+ mod1$coef[10]+ mod1$coef[11]+mod1$coef[12]
seb  <- sqrt(sum(mod1$Vp[9:12,9:12]))

exp(sumb*10)
exp((sumb -1.96*seb)*10)
exp((sumb +1.96*seb)*10)


# 12. fit a model with a 2-D smoothing for temperature and dew point temperature. 
# pers=True gives you the tri-dimensional plot.
 

mod2 <- gam(cvd ~ s(time,fx=TRUE,k=41)+s(TEMP,RH)+as.factor(dow)+ pm25, family=quasipoisson, data=nycpm, na.action=na.omit)
summary(mod2)


	plot( mod2, pers=TRUE,select=2)
	plot( mod2, pers=TRUE,select=2,theta=10,phi=10)
	plot( mod2, pers=TRUE,select=2,theta=50,phi=10)
	plot( mod2, pers=TRUE,select=2,theta=90,phi=10)
	plot( mod2, pers=TRUE,select=2,theta=50,phi=50)
