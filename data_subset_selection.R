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
lasso.coef
lasso.coef[lasso.coef != 0]

# the variables that are selected include lag1, lag2, lag5, volume.lag1, volume.lag4, volume.lag5,
# and volume.today.
