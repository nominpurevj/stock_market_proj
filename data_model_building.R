library(ISLR); library(tidyverse); library(class) 
library(MASS); library(boot); library(glmnet)

logreg.fit <- glm(up???lag1*volume.today+lag2*volume.lag1+lag5*volume.lag4,
                  data=spy.us ,family=binomial)
summary(logreg.fit)

# volume, lag1, and lag2 are signifant. lets see the training error rate
logreg.probs <- predict (logreg.fit,type="response")
logreg.pred <- rep(1, nrow(spy.us))
logreg.pred[logreg.probs >.5] <- 0
table(logreg.pred, spy.us$direction)
mean(logreg.pred == spy.us$up)
plot(logreg.probs, spy.us$up)
# when the actual direction is up, the model predicts correctlty 87% of the time
# when the actual direction is down, the model predicts correctly 19% of the time
# the error rate is 43.9%

# lets try the same but with only the significant variables and 
# use cross validation to get the test error rather than the training error.

logreg.fit <- glm(up~lag1+volume.today+lag2+volume.lag1+lag5+volume.lag4+volume.lag5,
                  data=spy.us ,family=binomial)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cv.error <- cv.glm(spy.us,logreg.fit, cost, K=5)$delta[1]
cv.error
summary(logreg.fit)
# the classification error rate is 44%


# lets try lda with 10-fold cross validation
spy_shuffled <- spy.us[sample(nrow(spy.us)),]
folds <- cut(seq(1,nrow(spy_shuffled)),breaks=10,labels=FALSE)

lda.cv.error <- rep(0, 10)
for(i in 1:10){
  #Segment your data by fold using the which() function 
  test <- which(folds==i,arr.ind=TRUE)
  testData <- spy_shuffled[test, ]
  train <- which(folds!=i,arr.ind=TRUE)
  lda.fit <- lda(up???lag1+lag2+Volume+volume.lag1+volume.lag2, data = spy_shuffled, subset = train)
  lda.pred <- predict(lda.fit, testData)
  lda.cv.error[i] <- mean(lda.pred$class != testData$up)
}

mean(lda.cv.error)
# the test error rate is 43.6%

# lets try using QDA.

qda.cv.error <- rep(0, 10)
for(i in 1:10){
  #Segment your data by fold using the which() function 
  test <- which(folds==i,arr.ind=TRUE)
  testData <- spy_shuffled[test, ]
  train <- which(folds!=i,arr.ind=TRUE)
  qda.fit <- qda(up???lag1+lag2+Volume, data = spy_shuffled, subset = train)
  qda.pred <- predict(qda.fit, testData)
  qda.cv.error[i] <- mean(qda.pred$class != testData$up)
}

mean(qda.cv.error)
# the test error rate is 44.15%