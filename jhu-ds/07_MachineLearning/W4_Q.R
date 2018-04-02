# Q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modRF <- train(y ~ ., data=vowel.train, method="rf") #, trControl=trainControl("cv"), number=3)
modBoost <- train(y ~ ., data=vowel.train, method="gbm", verbose=FALSE)

predRF <- predict(modRF, vowel.test)
predBoost <- predict(modBoost, vowel.test)
agreedIndex <- predRF == predBoost

cfmRf <- confusionMatrix(vowel.test$y, predRF)
cfmBoost <- confusionMatrix(vowel.test$y, predBoost)
cfmAgreed <- confusionMatrix(vowel.test$y[agreedIndex], predRF[agreedIndex])

cfmRf$overall["Accuracy"]
cfmBoost$overall["Accuracy"]
cfmAgreed$overall["Accuracy"]

# dataCombined <- data.frame(predRF, predBoost, y=vowel.test$y)
# modCombined <- train(y ~ ., data=dataCombined, method="gam", verbose=FALSE)
# predCombined <- predict(modCombined, dataCombined)
# cfmCombined <- confusionMatrix(predCombined, vowel.test$y)
# cfmCombined$overall["Accuracy"]

# Q2
rm(list=ls())


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
modRF2 <- train(diagnosis ~ ., data=training, method="rf") #, trControl=trainControl("cv"), number=3)
modBoost2 <- train(diagnosis ~ ., data=training, method="gbm", verbose=FALSE)
modLDA2 <- train(diagnosis ~ ., data=training, method="lda", verbose=FALSE)

predRF2 <- predict(modRF2, testing)
predBoost2 <- predict(modBoost2, testing)
predLDA2 <- predict(modLDA2, testing)

dataCombined <- data.frame(predRF2, predBoost2, predLDA2, diagnosis=testing$diagnosis)
modCombined <- train(diagnosis ~ ., data=dataCombined, method="rf", verbose=FALSE)
predCombined <- predict(modCombined, dataCombined)

cfmRF2 <- confusionMatrix(testing$diagnosis, predRF2)
cfmBoost2 <- confusionMatrix(testing$diagnosis, predBoost2)
cfmLDA2 <- confusionMatrix(testing$diagnosis, predLDA2)
cfmCombined <- confusionMatrix(testing$diagnosis, predCombined)


cfmRF2$overall["Accuracy"]
cfmBoost2$overall["Accuracy"]
cfmLDA2$overall["Accuracy"]
cfmCombined$overall["Accuracy"]

# Q3

rm(list=ls())

set.seed(3523)
library(AppliedPredictiveModeling)
library(elasticnet)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
modLasso <- train(CompressiveStrength ~ ., data=training, method="lasso")
plot.enet(modLasso$finalModel,  xvar="penalty", use.color=TRUE)


# Q4
library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

modBats <- bats(tstrain)
forecastObj <- forecast(modBats, level=95, h=nrow(testing))
betweenVal <- sum(testing$visitsTumblr > forecastObj$lower &  testing$visitsTumblr < forecastObj$upper)
betweenVal / nrow(testing) * 100

# Q5

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(e1071)

set.seed(325)
modSvm <- svm(CompressiveStrength ~ ., data = training)
predSvm <- predict(modSvm, testing)
accSvm <- accuracy(predSvm, testing$CompressiveStrength)
data.frame(accSvm)["RMSE"]
