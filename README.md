# Time-Series-Analysis

In this project I’m going to analyses a given time series, that is N225 (short name for Japan’s Nikkei 225 stock average), one of the most respected index of Japanese stocks. In particular we will:   

Give a general overview of the data, initially without any filters and then by considering the returns; 
Linear model estimation: estimate of ARIMA models and some diagnostic to evaluate the model;  
Non-linear model estimation: and estimate the best SETAR model and the best LSTAR model for the time series; 
Account for the residual heteroskedasticity and for kurtosis or skewness;  
Estimate the best (G)ARCH model and do some analysis on the residuals;  
Predict some observations in the future. 


Finally, after our brief analyses we can arrive to some conclusions. We have analyzed a time series, first of all by considering some transformations of the data in order to make it stationary, we have solved this issue with the so-called returns. After this the following steps was to try to use a linear model in order to analyze our dataset. The best ARIMA model estimate for our dataset was an ARIMA (1,0,0) therefore an AR(1). After the estimation of the best ARIMA model we also considered the possibility of using nonlinear models in order to treat our data, but at the end after some analysis, we chose to use the linear one. Then, we could have stopped at that point, since the residuals of the ARIMA model presented a desirable behavior (stationarity, and independence). 

However, in order to obtain better results, observing that there were still problems of leptokurtosis in the residuals of the model, we tried to apply a model for conditional heteroscedasticity, and then estimated the best model. The final residuals appear to be stationary, independent, gaussian distributed and we also reduced the leptokurtosis, reaching a result almost equal to the theoretical desirable one. At the end by by using the AIC criteria we can conclude that the best model is the GARCH one. The last step in our analysis was to do a prediciton su our model, but making a prediction su Arch model do not bring to good results for our dataset
