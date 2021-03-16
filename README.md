---
layout: post
title:  "Predicting S&P 500 Returns Based on Daily Stock Market Data"
date:   2021-02-25 21:42:18 -0800
categories: data science project
---

# Introduction
Predicting the ups and downs of the stock market has long been an extremely difficult problem to solve. Even after the insurgence of machine learning, most have not been successful at creating a model that predicts the stock market at a satisfactory level. Some even argue that the complexity of market makes it impossible to predict the market with enough efficiency.[^1]

Nevertheless, the stock market continues to produce massive amounts of information that can be used by data scientists to build more complex machine learning models and improve upon previously tried algorithms.

The present analysis uses data from the **SPDR S&P 500 ETF** to predict whether the price of the ETF (exchange traded fund) on a given day can be predicted by previous days' prices and volumes of transaction. The SPDR S&P 500 ETF is a fund tracks the Standard & Poor's 500 Index, which comprises 500 large and mid-cap U.S. stocks.[^2]

### Background
On this analysis, I extend the methods in the book *Introduction to Statistical Learning (ISLR)* by Witten, Tibshirani, Hastie, and M. James. I obtained a dataset from Kaggle[^3], that is similar to book's *Weekly* dataset. 

The data features trading prices from the SPDR S&P 500 ETF captured at the opening and closing of the stock market, as well as at the maximum and minimum trading prices throughout each day from February 25th, 2005 to November 10th, 2017. Moreover, the dataset contains the trading volume, which indicates how many times the SPDR S&P 500 ETF has been traded on each given day.

#### A preview of the data before cleaning:
![S&P 500 SPY ETF daily returns data](/assets/images/spy_us_data_before.JPG)

>This write-up is most suited to be understood by those who are already knowledgeable about machine learning methods. However, if you are unfamiliar with machine learning, you may still follow along with the following information in mind:
>1. The present analysis uses **classification** to predict whether the price of the S&P 500 ETF will go up or down on a given day.
>2. There are many different ways to predict the ups and downs. Logistic Regression, LDA, and QDA are commonly used for this task. This analysis tries to find which of the three is best for this scenario.
>4. In all methods, a portion of the stock market data is responsible for the code that learns how to classify. The other portion is used to confirm if the code is classifying well-enough. **10-fold cross validation** helps us carry out this train-and-test procedure 10 times, for safety.  
>3. The 10-fold cross validation gives us the **test errors**, which is what I'm gonna compare. When it comes to test error, lower is better.


# Data Cleaning 
```r
spy.us <- read.csv("~/Projects/stock_market_proj/spy.us.txt")
```
```r
# Use open and close to create percentage returns for the present day.
spy.us['today'] <- round(spy.us$Close / spy.us$Open * 100 - 100, 3)

# Create lags that indicate the previous days' percentage return.
# The first 5 days of the dataset will not be used as response variables.

spy.us['lag1'] <- c(NA, spy.us$today[-3201]) 
spy.us['lag2'] <- c(NA, spy.us$lag1[-3201]) 
spy.us['lag3'] <- c(NA, spy.us$lag2[-3201]) 
spy.us['lag4'] <- c(NA, spy.us$lag3[-3201]) 
spy.us['lag5'] <- c(NA, spy.us$lag4[-3201]) 

spy.us['volume.lag1'] <- c(NA, spy.us$Volume[-3201]) 
spy.us['volume.lag2'] <- c(NA, spy.us$volume.lag1[-3201]) 
spy.us['volume.lag3'] <- c(NA, spy.us$volume.lag2[-3201]) 
spy.us['volume.lag4'] <- c(NA, spy.us$volume.lag3[-3201]) 
spy.us['volume.lag5'] <- c(NA, spy.us$volume.lag4[-3201]) 

# add year as a variable in order to replace "date", which is too specific
spy.us['year'] <- as.numeric(substring(spy.us$Date, 1, 4))

# reorder the columns
col_order <- c("year", "lag1", "lag2", "lag3", "lag4", "lag5", 
               "volume.lag1", "volume.lag2", "volume.lag3", "volume.lag4", "volume.lag5", 
               "Volume", "today", "Date", "Open", "High", "Low", "Close", "OpenInt")
spy.us <- spy.us[, col_order]
names(spy.us)[names(spy.us) == "Volume"] <- "volume.today"

# get rid of irrelevant variables
spy.us <- spy.us[6:3201, 1:13]
rownames(spy.us) <- NULL 

# add outcome variable
spy.us['direction'] <- rep("down", nrow(spy.us))
spy.us$direction[spy.us$today >= 0] <- "up"

# create dummy outcome variable
spy.us$up <- 0
spy.us$up[spy.us$direction == 'up'] <- 1
```
The cleaned data now contains `12` explanatory variables.

#### A preview of the data after cleaning:
![S&P 500 SPY ETF daily returns data cleaned](/assets/images/spy_us_data_after_1.JPG)
![S&P 500 SPY ETF daily returns data cleaned](/assets/images/spy_us_data_after_2.JPG)

# Data Exploration 

Let's create a scatterplot matrix to see which variables are related and to get a general sense of how each variable is distributed.
```r
library(GGally)
ggscatmat(spy.us, color = "direction")
```
![Scatterplot matrix for S&P 500 daily return data](/assets/images/scatterplot_matrix.png)

Understandably, strong correlations are observed among the volumes of a given day and the days immediately preceding it. For instance, `volume.lag2` and `volume.lag3` have a correlation of `0.85`.

Moreover, `volume.lag1` and `lag2` have a weak correlation. Even though the correlation is weak, it is worth noting as most variables are not correlated at all. The same goes for `volume.lag2` and `lag3`, and `volume.lag3` and `lag4`. They hint that smaller returns in the market on a given day are associated with higher volume of trading on the next day.

Let's take a look at the relationship between volume and returns to get a clearer understanding of the correlation.

```r
ggplot(spy.us, aes(lag2, volume.lag1)) +
  geom_point() + 
  geom_smooth(se = FALSE)
```
![Scatterplot for volume.lag1 and lag2](/assets/images/volume_return_plot.png)

This graph makes it clear why there is a negative associated between volume and the previous day's returns. 

It does not necessarily seem to be the case that lower return on a given day is associated with more activity the next day. What more seems to be the case is that **both high very low and very high returns on a given day are associated with high volumes of exchange the next day**. However, the skew is more towards the low returns.

# Subset Selection

The current dataset, named `spy.us` contains a total of 14 explanatory variables. Some of these variables may not only be useless for the classification, but they may also decrease the quality of the classification model. This means that only the most helpful set of variables should be selected for the machine learning model. 

In this analysis, I use a method named **LASSO** (Least Absolute Shrinkage and Selection Operator) that selects a subset of variables by choosing the variables that minimizes prediction error. 

```r
library(glmnet)

# create a matrix corresponding to the predictors and outcomes
x=model.matrix(direction~., spy.us)[,c(2:13)]
y=spy.us$up

# set up test and train datasets
set.seed(1)
train=sample (1: nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# make sure lasso is over the range of values lambda = 10**10 to lambda = 10**2
grid=10^seq(10,-2, length =100)
lasso.mod=glmnet(x[train ,],y[ train],alpha=1, family=binomial, lambda =grid)

# implement lasso and find which lambda performs best with cross-validation
cv.out=cv.glmnet(x[train ,],y[ train],alpha=1, family=binomial)
bestlam =cv.out$lambda.min
out=glmnet(x,y,alpha=1, lambda=grid)

# find which coefficients should be kept
lasso.coef=predict(out, type='coefficients', s=bestlam) [1:13,]
lasso.coef[lasso.coef != 0]
```
Lasso has selected the following `7` variables: `lag1`, `lag2`, `lag5`, `volume.lag1`, `volume.lag4`, `volume.lag5`, and `volume.today`.

# Model Building

As mentioned earlier, the present analysis compares Logistic Regression, Linear Discriminant Analysis, and Quadratic Discriminant Analysis. 10-fold cross validation is used to see which method results in the smallest test error rate.

### Logistic Regression

Let's use the variables from the subset selection to create a logistic regression model and then get the test error rate.
```r
logreg.fit <- glm(up~lag1+lag2+lag5+volume.today+volume.lag1+volume.lag4+volume.lag5,
                  data=spy.us ,family=binomial)
summary(logreg.fit)

# get the test error 
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cv.error <- cv.glm(spy.us,logreg.fit, cost, K=10)$delta[1]
cv.error
```
The test error rate is `41.3%`

### Linear Discriminant Analysis (LDA)

Let's use the same variables to build a model using LDA. In order to do this with 10-fold cross-validation, the data needed to be shuffled and randomly divided into 10 sections. 
```r
spy_shuffled <- spy.us[sample(nrow(spy.us)),]
folds <- cut(seq(1,nrow(spy_shuffled)),breaks=10,labels=FALSE)

lda.cv.error <- rep(0, 10)
for(i in 1:10){
  #Segment data by fold using the which() function 
  test <- which(folds==i,arr.ind=TRUE)
  testData <- spy_shuffled[test, ]
  train <- which(folds!=i,arr.ind=TRUE)
  lda.fit <- lda(up~lag1+lag2+lag5+volume.today+volume.lag1+volume.lag4+volume.lag5, data = spy_shuffled, subset = train)
  lda.pred <- predict(lda.fit, testData)
  lda.cv.error[i] <- mean(lda.pred$class != testData$up)
}

mean(lda.cv.error)
```
The results have yielded that the test error rate is `41%`

### Quadratic Discriminant Analysis (QDA)

The same procure is repeated here for QDA and the performance is assessed with 10-fold cross-validation.

```r
qda.cv.error <- rep(0, 10)
for(i in 1:10){
  #Segment data by fold using the which() function 
  test <- which(folds==i,arr.ind=TRUE)
  testData <- spy_shuffled[test, ]
  train <- which(folds!=i,arr.ind=TRUE)
  qda.fit <- qda(up~lag1+lag2+lag5+volume.today+volume.lag1+volume.lag4+volume.lag5, data = spy_shuffled, subset = train)
  qda.pred <- predict(qda.fit, testData)
  qda.cv.error[i] <- mean(qda.pred$class != testData$up)
}

mean(qda.cv.error)
```
The test error rate here is `44%`

# Results & Conclusion
In conclusion, Logistic Regression performed very similarly to LDA at a test error rate of about `41%`.  QDA produced the worst performance at a test error rate of `44%`. This result is likely due to the fact that some of the explanatory variables are distributed similarly to that of the t-distribution, and were barely correlated. Both LDA and Logistic Regression can work well with these conditions, but QDA cannot.

Even though the test error rate is high at `41%`, there is still some improvement over pure chance (`50%`), leaving the door open for future models to use similar variables. Combined with other observations from within the stock market and outside of the stock market, the model may be able to decrease its test error rate further.  

# References

[^1]: [Why We Can’t Predict Financial Markets](https://hbr.org/2009/01/why-we-cant-predict-financial "Harvard Business Review"). Harvard Business Review. 2009.

[^2]: [SPY: SPDR S&P 500 Trust ETF](https://www.investopedia.com/articles/investing/122215/spy-spdr-sp-500-trust-etf.asp "Investopedia"). Investopedia. 2020.

[^3]: [Huge Stock Market Dataset](https://www.kaggle.com/borismarjanovic/price-volume-data-for-all-us-stocks-etfs "Kaggle"). *Historical daily prices and volumes of all U.S. stocks and ETFs*. Kaggle. 2018.

