library(ISLR); library(tidyverse); library(class) 
library(MASS); library(boot)

set.seed(10)

### LOGISTIC REGRESSION ###

# use the variables from the subset selection to create a logistic regression model
logreg.fit <- glm(up~lag1+lag2+lag5+volume.today+volume.lag1+volume.lag4+volume.lag5,
                  data=spy.us ,family=binomial)
summary(logreg.fit)

# use 10-fold cross validation to get the test error rather than the training error.
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cv.error <- cv.glm(spy.us,logreg.fit, cost, K=10)$delta[1]
cv.error

# the classification test error rate is 41.3%

### LINEAR DISCRIMINANT ANALYSIS ###

# lets shuffle and section the data so that the indices are randomly divided into 10 sections
# for the 10-fold cross-validation.
spy_shuffled <- spy.us[sample(nrow(spy.us)),]
folds <- cut(seq(1,nrow(spy_shuffled)),breaks=10,labels=FALSE)

lda.cv.error <- rep(0, 10)
for(i in 1:10){
  #Segment your data by fold using the which() function 
  test <- which(folds==i,arr.ind=TRUE)
  testData <- spy_shuffled[test, ]
  train <- which(folds!=i,arr.ind=TRUE)
  lda.fit <- lda(up~lag1+lag2+lag5+volume.today+volume.lag1+volume.lag4+volume.lag5, data = spy_shuffled, subset = train)
  lda.pred <- predict(lda.fit, testData)
  lda.cv.error[i] <- mean(lda.pred$class != testData$up)
}

mean(lda.cv.error)
# the test error rate is 41%

### QUADRATIC DISCRIMINANT ANALYSIS ###

# the procedure for LDA is repeated here for QDA
qda.cv.error <- rep(0, 10)
for(i in 1:10){
  #Segment your data by fold using the which() function 
  test <- which(folds==i,arr.ind=TRUE)
  testData <- spy_shuffled[test, ]
  train <- which(folds!=i,arr.ind=TRUE)
  qda.fit <- qda(up~lag1+lag2+lag5+volume.today+volume.lag1+volume.lag4+volume.lag5, data = spy_shuffled, subset = train)
  qda.pred <- predict(qda.fit, testData)
  qda.cv.error[i] <- mean(qda.pred$class != testData$up)
}

mean(qda.cv.error)
# the test error rate is 44.15%


# In conclusion, Logistic Regression performed very similarly to LDA. QDA produced the worst performance.