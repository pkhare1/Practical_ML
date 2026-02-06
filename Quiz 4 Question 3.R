set.seed(3523)
library(AppliedPredictiveModeling)
library(glmnet)

data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

# Fitting lasso model using caret package (this method doesn't retain lambda paths so we will not end up using this)
lassoModel <- train(
  CompressiveStrength ~ .,
  data = training,
  method = "glmnet",
  tuneGrid = expand.grid(
    alpha = 1,
    lambda = seq(0.0001, 1, length = 50)
  )
)

# Fitting lasso model using glmnet package directly
# the model.matrix gives us the entire training dataset (except compressive strength
# in numeric matrix form and we add [,-1] to get rid of the first column which is
# the intercept column (all 1s)
x <- model.matrix(CompressiveStrength ~ ., data = training)[,-1]
y <- training$CompressiveStrength
lassoFit <- glmnet(x, y, alpha = 1)
coefMat <- as.matrix(coef(lassoFit))[,-1] #a matrix of rows = predictors and columns = no. of lambda values tested by glmnet
coefMat <- coefMat[rownames(coefMat) != "(Intercept)", ] #removing the intercept row

# this operation below proceeds row wise. For each row (or predictor) it will filter out those coefficients which are 0
# and only retain the corresponding lambda values which correspond to non-zero coefficients, then take the max
lastNonZeroLambda <- apply(coefMat, 1, function(coefs) {
  max(lassoFit$lambda[coefs != 0])
})
lastVar <- names(which.max(lastNonZeroLambda))
lastVar
# Answer is lastVar

