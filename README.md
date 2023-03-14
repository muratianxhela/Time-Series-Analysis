**Time-Series-Analysis**

**In this project I’m going to analyses a given time series, that is N225 (short name for Japan’s Nikkei 225 stock average), one of the most respected index of Japanese stocks. In particular we will:**   

- Give a general overview of the data, initially without any filters and then by considering the returns; 
- Linear model estimation: estimate of ARIMA models and some diagnostic to evaluate the model;  
- Non-linear model estimation: and estimate the best SETAR model and the best LSTAR model for the time series; 
- Account for the residual heteroskedasticity and for kurtosis or skewness;  
- Estimate the best (G)ARCH model and do some analysis on the residuals;  
- Predict some observations in the future. 

OBJECTIVE OF THE ANALYSIS


1.Give a general overview of the data, initially without any filters and then by considering the returns;
2. Linear model estimation: estimate of ARIMA models and some diagnostic to evaluate the
model;
3. Non-linear model estimation: and estimate the best SETAR model and the best LSTAR
model for the time series;
4.account for the residual heteroskedasticity and for kurtosis or skewness;
5.estimate the best (G)ARCH model and do some analysis on the residuals;
6.predict some observations in the future.

First of all, we have to load the necessary libraries.


```
install.packages("quantmod")
Error in contrib.url(repos, "source"): trying to use CRAN without setting a mirror
library(quantmod)
library(forecast)
library(moments)
library(lmtest)
library(car)
library(tseries)
library(tsDyn)
rm(list=ls())
getSymbols("^N225", source="yahoo")
[1] "^N225"
N225 -> TS6
names(TS6) <- c("Open","High","Low","Close","Volume","Adjusted")
dat.TS<-TS6$Adjusted
```
Simple imputation library 
```
install.packages("imputeTS")
library(imputeTS)
sum(is.na(dat.TS))
```
We find out that there are 24 missing values in our dataset that may infer our predictions for the future. For this reason we need to adress this problem as follows:
```
na_interpolation(dat.TS)-> dat.TS
sum(is.na(dat.TS))
```
> Descriptive Analysis

At this poin we can do a graphical representation of the time series behaviour and the analysis of the autocorrelation function and of the partial autocorrelation
function.
```
plot.ts(dat.TS)
acf(dat.TS, lag.max = NULL, type = "correlation", plot = TRUE, na.action = na.fail,
    demean = TRUE)
pacf(dat.TS, lag.max=NULL, plot=TRUE, na.action=na.fail)

adf.test(dat.TS)# non stationary
```
#####  Unit-Root identification and Returns descriptive analysis####

logdata <- log(dat.TS)
plot.ts(logdata)
adf.test(logdata) # non stationary

return <- as.vector(diff(logdata)[-1])
plot(as.ts(return))
acf(return, lag.max = NULL, type = "correlation", plot = TRUE, na.action = na.pass,
    demean = TRUE)
pacf(return, lag.max=NULL, plot=TRUE, na.action=na.pass)

#####Formal tests###

adf.test(return) #stationary
pp.test(return)
par(mfrow=c(3,1))
plot.ts(dat.TS)
plot.ts(logdata)
plot.ts(return)
par(mfrow=c(1,1))
durbinWatsonTest(lm(return~1), max.lag = 4)
dwtest(lm(return~1), alternative="two.sided")## p.value is  equal to 0.055 we can accept the null, so there is absence of serial
#autocorrelation.



######Verify the pattern of the time series of the returns#####
Box.test(dat.TS)
Box.test(return)#Given the following results, in both cases, this test suggest that the null hypothesis should be
 ##rejected, so the values are showing dependence on each other.


hist((return-mean(return))/sd(return),nclass=20,freq=F)
lines(seq(-5,5,0.01), dnorm(seq(-5,5,0.01)))
jarque.bera.test(return)
shapiro.test(return)#the p.value is very low so we need to reject the null hypothesis, so we
#arrive to the conclusion that there is not a normal distribution.The same result is also confirmed
#by Shapiro-Wilks test moreover, this result is also clear by seeing the plo

qqnorm(return)
qqline(return)

n<-length(return)
mean(return)# is very small
skewness(return)
kurtosis(return)-3
## kurtosis is equal to 8.07 that is greater then 3 and this indicates the presence of leptokurtosis (as we have
#already intuited from the histogram)
#####Lag plot####
lag.plot(return)

####ARIMA models estimation####


auto.arima(return)
bestAIC<-auto.arima(return, max.p=3, max.q=3, ic = "aic", stepwise=F, allowmean=FALSE, allowdrift = FALSE)
bestAIC
bestBIC<-auto.arima(return, max.p=3, max.q=3, ic = "bic", stepwise=F, allowmean=FALSE, allowdrift = FALSE)
bestBIC
### we model we obtain is  ARIMA(1,0,0), therefore an AR(1).
#####Residuals diagnosis###
plot(bestAIC$res)
hist(bestAIC$res, freq=F)
lines(seq(-2,2,0.01), dnorm(seq(-2,2,0.01), mean(bestAIC$res), sd(bestAIC$res)))
acf(bestAIC$res)
pacf(bestAIC$res)

######Let's see more details on the histogram plot###

hist(bestAIC$res, freq=F, nclass=30)
#A normal distribution
lines(seq(-2,2,0.01), dnorm(seq(-2,2,0.01), mean(bestAIC$res), sd(bestAIC$res)))

qqnorm(bestAIC$res)
qqline(bestAIC$res)
kurtosis(bestAIC$res)
skewness(bestAIC$res)
##As we can see from the results, we have still leptokurtosis distribution.

####Formal tests###


Box.test(bestAIC$residuals)#no serial correlation
adf.test(bestAIC$residuals) #stationary
jarque.bera.test(bestAIC$residuals)# non normality
tsdiag(bestAIC)
##The Box-Pierce test indicates that we have to accept the null hypotesis of no correlation and the
##Augmented Dickey Fuller test proves that the residuals are stationary (which is not surprising, since we had a stationary process).

######S-ARIMA models####

plot(as.ts(return))


##### Non-linear models: threshold models

######SETAR models####
selectSETAR(return, d=1, m=3, mL=2, mH=2, thSteps=10, thDelay=0:2)
selectSETAR(return, d=1, m=5, mL=5, mH=5, thSteps=10, thDelay=0:2)
modSETAR <- setar(return, d=1, m=2, mL=1, mH=1, thDelay=1)
summary(modSETAR)
plot(modSETAR)
Box.test(modSETAR$resid) #no serial correlation
jarque.bera.test(modSETAR$resid) # non normality

adf.test(modSETAR$resid) #stationary
e <- residuals(modSETAR)
plot(e)
## we can see that we have no serial correlation, stationary, and the
##residuals are not normally distributed

####check for independency between estimated values and the residuals of the
##model.

rend.hat <- fitted(modSETAR)# the estimated value
plot(rend.hat, e)
cor.test(rend.hat, e, use="complete")

######LSTAR models######

modLSTAR <- lstar(return, m=3, d=1, mL=1, mH=1, thDelay=1)

summary(modLSTAR)

######Choice of the model#####

#####Now we have to choose between two different models:
#an ARIMA(1,0,0),therefore an AR(1)
## a SETAR model

plot(bestAIC$residuals)
plot.ts(modSETAR$residuals)
####Since the two models bring to the same results, we can choose the simplest one, therefore an
###ARIMA(1,0,0)##

#######Conditional heteroscedasticity models: GARCH models###
bptest(return[-1]~ return[-length(return)])
bptest(lm(bestAIC$res[-1]~bestAIC$res[-length(bestAIC$res)]))
kurtosis(bestAIC$residuals)
mod01<-garch(bestAIC$res, order=c(0,1), trace=T)

summary(mod01)

### more complicated model ###
mod02<-garch(bestAIC$res, order=c(0,2), trace=F)
summary(mod02)
mod03<-garch(bestAIC$res, order=c(0,3), trace=F)
summary(mod03)
mod04<-garch(bestAIC$res, order=c(0,4), trace=F)
summary(mod04)


#####First of all, we consider AIC###

AIC(mod01)
AIC(mod02)
AIC(mod03)
AIC(mod04)# the best
####And then we can consider BIC###

AIC(mod01, k = log(mod01$n.used))

AIC(mod02, k = log(mod01$n.used))

AIC(mod03, k = log(mod01$n.used))
AIC(mod04, k = log(mod01$n.used))
## the best model is the last model  considering AIC and BIC criteria 
plot(mod04)
#At this point, we can also consider GARCH models and try to understand if they are better then
#the simple ARCH
mod11<-garch(bestAIC$res, order=c(1,1), trace=T)
summary(mod11)

mod12<-garch(bestAIC$res, order=c(1,2), trace=T)
summary(mod12)
AIC(mod01)

AIC(mod02)

AIC(mod11)# best one

par(mfrow=c(1,1))
qqnorm(bestAIC$resid)
qqline(bestAIC$resid)
qqnorm(mod01$resid)
qqline(mod01$resid)kurtosis(bestAIC$resid)

kurtosis(mod01$resid[3:199])
#### see the distribution of the residuals

hist((mod01$resid[3:199]-mean(mod01$resid[3:199]))/sd(mod01$resid[3:199]),nclass=20,freq=F)
lines(seq(-5,5,0.01), dnorm(seq(-5,5,0.01)))

Finally, after our brief analyses we can arrive to some conclusions. We have analyzed a time series, first of all by considering some transformations of the data in order to make it stationary, we have solved this issue with the so-called returns. After this the following steps was to try to use a linear model in order to analyze our dataset. The best ARIMA model estimate for our dataset was an ARIMA (1,0,0) therefore an AR(1). After the estimation of the best ARIMA model we also considered the possibility of using nonlinear models in order to treat our data, but at the end after some analysis, we chose to use the linear one. Then, we could have stopped at that point, since the residuals of the ARIMA model presented a desirable behavior (stationarity, and independence). 

However, in order to obtain better results, observing that there were still problems of leptokurtosis in the residuals of the model, we tried to apply a model for conditional heteroscedasticity, and then estimated the best model. The final residuals appear to be stationary, independent, gaussian distributed and we also reduced the leptokurtosis, reaching a result almost equal to the theoretical desirable one. At the end by by using the AIC criteria we can conclude that the best model is the GARCH one. The last step in our analysis was to do a prediciton su our model, but making a prediction su Arch model do not bring to good results for our dataset
