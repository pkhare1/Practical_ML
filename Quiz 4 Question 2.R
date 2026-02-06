# Load required packages
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

# Objective 1: Fit the training model using random forest, 
# boosted trees and linear discriminant analysis

trainrf <- train(diagnosis ~ ., data = training, method = "rf")
traingbm <- train(diagnosis ~ ., data = training, method = "gbm", verbose = FALSE)
trainlda <- train(diagnosis ~ ., data = training, method = "lda")

# Objective 2: Predicting on the test set for each model
predrf <- predict(trainrf, newdata = testing)
predgbm <- predict(traingbm, newdata = testing)
predlda <- predict(trainlda, newdata = testing)

# Objective 3: Build a dataset that combines these predictions with the 
# testing "diagnosis" data
pred_df <- data.frame(predrf, predgbm, predlda, 
                      diagnosis = testing$diagnosis)

# Objective 4: train and predict stacked model using RF
stackedtrain <- train(diagnosis ~ ., data = pred_df, method = "rf")
stackedpredrf <- predict(stackedtrain, newdata = pred_df)

# Objective 5: comparing all the accuracy
rfaccuracy <- confusionMatrix(predrf, testing$diagnosis)$overall["Accuracy"]
gbmaccuracy <- confusionMatrix(predgbm, testing$diagnosis)$overall["Accuracy"]
ldaaccuracy <- confusionMatrix(predlda, testing$diagnosis)$overall["Accuracy"]
stackedaccuracy <- confusionMatrix(stackedpredrf, testing$diagnosis)$overall["Accuracy"]