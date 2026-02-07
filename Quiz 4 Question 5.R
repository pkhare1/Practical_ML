set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)

data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

mod1 <- svm(CompressiveStrength ~ ., data = training)
pred1 <- predict(mod1, newdata = testing)
RMSE <- sqrt(mean((pred1 - testing$CompressiveStrength)^2))