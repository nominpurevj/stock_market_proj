library(ISLR)
library(GGally)
library(tidyverse)
library(class) 
library(MASS)

data = Weekly


# (a) Produce some numerical and graphical summaries of the Weekly
# data. Do there appear to be any patterns?

summary(Weekly)
ggscatmat(Weekly, color = "Direction")

ggplot(Weekly, aes(Year, Volume)) +
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(Weekly, aes(Lag2, up))+
  geom_point() + 
  geom_smooth(se = FALSE)

# There are more instances where the percentage return on investment has gone up (605) in the week than down (484).
# there is a strong positive correlation between volume and year. 
# volume is increasing over time


# (b) Use the full data set to perform a logistic regression with
# Direction as the response and the five lag variables plus Volume
# as predictors. Use the summary function to print the results. Do
# any of the predictors appear to be statistically significant? If so,
# which ones?
Weekly$up <- 0
Weekly$up[Weekly$Direction == 'Up'] <- 1

glm_fit <- glm(up???Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                           data=Weekly ,family=binomial )
summary(glm_fit)

# smallest p-value appears to be significantly associated with a positive return. 
# meaning that if the market had a positive return 2 weeks ago, it will more likely 
# go up today.

# (c) Compute the confusion matrix and overall fraction of correct
# predictions. Explain what the confusion matrix is telling you
# about the types of mistakes made by logistic regression.

glm.probs=predict (glm_fit,type="response")
glm.probs [1:10]
glm.pred=rep("Down", 1089)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred, Weekly$Direction)

correctness <- (54+557)/1089

training_error <- 1- correctness

# so the training error rate is 43.8%

# (d) Now fit the logistic regression model using a training data period
# from 1990 to 2008, with Lag2 as the only predictor. Compute the
# confusion matrix and the overall fraction of correct predictions
# for the held out data (that is, the data from 2009 and 2010).

train <- (Weekly$Year < 2009)
Weekly.test = Weekly[!train,]
Weekly.train = Weekly[train,]
dim(Weekly.test)

glm_fit_validation <- glm(up???Lag2, data=Weekly.train ,family=binomial)

summary(glm_fit_validation)

glm.probs= predict(glm_fit_validation, Weekly.test, type="response")
glm.pred=rep("Down", 104)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred, Weekly.test$Direction)

correctness <- (9+56)/104

test_error <- 1- correctness

# the test error is 37.5% for this model.

# (e) Repeat (d) using LDA.

lda_wkly <- lda(Direction ~ Lag2, data = Weekly, subset = train)
lda_wkly
plot(lda_wkly)
lda_probs <- predict(lda_wkly, Weekly.test)
table(lda_probs$class, Weekly.test$Direction)
mean(lda_probs$class == Weekly.test$Direction)

# the test error rate is 37.5%

# (f) Repeat (d) using QDA.

qda_wkly <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda_wkly
qda_pred <- predict(qda_wkly, Weekly.test)
table(qda_pred$class, Weekly.test$Direction)
mean(qda_pred$class == Weekly.test$Direction)

# the test error rate is 41.3% 

# (g) Repeat (d) using KNN with K = 1.

train_X <- as.matrix(Weekly$Lag2[train])
test_X <- as.matrix(Weekly$Lag2[!train])

set.seed(1)
knn_pred <- knn(train_X, test_X, Weekly.train$Direction, k = 1)
table(knn_pred, Weekly.test$Direction)
mean(knn_pred == Weekly.test$Direction)

# the test error rate is 50%

# (h) Which of these methods appears to provide the best results on
# this data?


#   (i) Experiment with different combinations of predictors, including possible transformations and interactions, for each of the
# methods. Report the variables, method, and associated confusion matrix that appears to provide the best results on the held
# out data. Note that you should also experiment with values for
# K in the KNN classifier.

# try the LogReg again but with all the variables since LogReg 
# doesnt require the normality assumption and LogReg would still 
# work on uncorrelated random variables

logreg_fit <- glm(up???Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                          data=Weekly.train ,family=binomial)
logreg.probs= predict(logreg_fit, Weekly.test, type="response")
logreg.pred=rep("Down", 104)
logreg.pred[logreg.probs >.5]=" Up"
table(logreg.pred, Weekly.test$Direction)
mean(logreg.pred == Weekly.test$Direction)

## the error rate is 70%

## try KNN with 5-fold CV 

k_cv <- 5 

Weekly_shuffled <- Weekly[sample(nrow(Weekly)),] ## shuffled data


#Create 5 equally size folds
folds <- cut(seq(1,nrow(Weekly_shuffled)),breaks=k_cv,labels=FALSE)

set.seed(17)
cv.error.k_cv=rep(0,k_cv)
for (i in 1:k_cv){
  testIndices <- which(folds==i,arr.ind=TRUE)
  testData <- as.matrix(Weekly_shuffled[testIndices, ]$Lag2)
  trainData <- as.matrix(Weekly_shuffled[-testIndices, ]$Lag2)
  trainResponse <- t(Weekly_shuffled[-testIndices,]['Direction'])
  testResponse <- t(Weekly_shuffled[testIndices,]['Direction'])
  knn_pred <- knn(trainData, testData, trainResponse, k = 1)
  cv.error.k_cv[i]= mean(knn_pred == testResponse)
  }

mean(cv.error.k_cv)

# error rate is 51%
