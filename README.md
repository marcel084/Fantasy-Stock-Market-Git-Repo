# Fantasy Stock Market Git Repository
Stock Market Project API
This project is a version of the Fantasy Stock Game. A key generated in alphavantage.co's API is used for checking stock prices and making transactions. The number of price look-ups is limited  with Alpha Vantage, API number of calls are limited and must be made made at the optimum time.
[Alpha Vantage Documentation](https://www.alphavantage.co/documentation/) (Links to an external site.)

### Requirements of the model:
All trading must be done from R and completely automated, running a script in a Virtual Machine hosted in AWS
Number of calls are limited to 2 calls per minute, maximum 300 API calls per day.
AT least 250 successful trades (buy/sell) must be done per day.
All trading is to be done between 10am-4pm.
Failed transactions will result in penalties
There is a minimum of 250 successful transactions per day. Any accounts below that threshold will be penalized after the market closes that day. The penalty will be $1000 per missed transaction. For instance, 10 unsuccessful transactions in a day, is equivalent to $10,000.
The user linked to the key has a budget of 100000 Big Data Dollars. The total value is composed by Value Shares and Budget Available. Penalties are substracted of the Total

### R.script
A Windows instance was created in EC2 instance(free tie) hosted AWS to run the script meeting all the previous requirements.
Starting the Day with a Time Series Analysis for 20 stock Symbol stored with a Forecasting for the following hour. This analysis is repeated every hour for a prediction of 60 values(each min of the following hour).
The Model chooses the maximum Profit value (Current pricing-Last Transaction Price) to sell and minimum Profit Value to Buy. Once this share is chosen, the current value is compared with the Forecasting Pricing average obtained in previous Time Series Analysis, just to check if the decision if buying/selling or wait and passing to the 2nd option.
The quantities set in every transaction is determined by Max value to spend of $3000.
The model meet the requirement of buying if there is Budget available and selling if there is more than 1 stock in available the portfolio

### Model Performance and Testing 1st Model Time Series  
This model run for 21 days, with an increasing balance of 0.02% compared to the initial budget. The condition for comparing the current price, before making the transaction, with the Time Series didn't allow to wait, the Forecasting values using Arima were not accurate(rmse of 8 and 14 USD) for Tesla and Amazon stocks test between actual and predicted values. Also, trend in the prediction is constant with no seasonality component forecasted.

### Recomendation and 2nd model in .Rmd
After having seen the results, a 2nd algorithm for another modelt was built but it wasn't tested in real time. This algorithm was based on each stock analysis in current time and then decide(classification) what to do with the stock in 4 different classes, Buy|Sell "safe" or Buy|Sell "big". The variables taken into account were past perfomance of the stock and a class determined through the forecasting of a share value if price is going up or down for the next observation, limiting this way the error in the forecasting.
The classification was done using  real values of the following observations. In current time this prediction is done by Time Series Analysis but since we have real values for future observations i the training set, the analysis was done based on this data. At the end of this Analysis predicted class based on real values are compared to predicted class based on Time Series Analyisis.
The classes were determined using the Models Regression and Classification Trees and Naive Bayes. Since the thresholds were especifically determined using quadrants and mean in the normalized data, the accuracy for Decision Tree Model was higher than Naive Bayes Model and it was close to 1(Fig2).
The previous result means that the whole classification model is going to be based on how good is the prediction of the class market up|down based on trend for the stock pricing in the following observation.
This class up/down determined through the following estimated price in Arima forecasting, was compared to what happened in real time in the data collected for past observations. The Accuracy for this prediction was tested and values obtained to predict if the market is going down/up in the following observations is around 64 and 71% for 26 observations of testing set of 780 observations.

### Libraries required in R
`library(tidyverse)`
`library(alphavantager)`
`library(forecast)`
`library(caTools)`
`library(rpart)`
`library(forecast)`
`library(naivebayes)`
`library(e1071)`
`library(randomForest)`
`library(partykit)`
`library(rpart.plot)`



