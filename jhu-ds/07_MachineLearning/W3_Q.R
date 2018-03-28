library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)

subset <- split(segmentationOriginal, segmentationOriginal$Case)
set.seed(125)
modCART <- rpart(Class ~ ., data=subset$Train)
modCART

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
testA <- segmentationOriginal[0,]
testA[1,c("TotalIntenCh2", "FiberWidthCh1", "PerimStatusCh1")] <- c(23000, 10, 2)
predict(modCART, testA, type="prob")

# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
testB <- segmentationOriginal[0,]
testB[1,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(50000, 10, 100)
predict(modCART, testB, type="prob")

# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
testC <- segmentationOriginal[0,]
testC[1,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(57000, 8, 100)
predict(modCART, testC, type="prob")

# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
testD <- segmentationOriginal[0,]
testD[1,c("FiberWidthCh1", "VarIntenCh4","PerimStatusCh1")] <- c(8, 100, 2)
predict(modCART, testD, type="prob")

# Q3

rm(list=ls())

library(pgmm)
data(olive)
olive = olive[,-1]

set.seed(125)
modCART2 <- rpart(Area ~ ., data=olive)
modCART2

newdata = as.data.frame(t(colMeans(olive)))
predict(modCART2, newdata)


# Q4

rm(list=ls())

library(ElemStatLearn)
library(caret)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

set.seed(13234)
fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data=trainSA, method="glm", family="binomial")

predictTrainSA <- predict(fit)
missClass(trainSA$chd,predictTrainSA)

predictTestSA <- predict(fit, testSA)
missClass(testSA$chd,predictTestSA)

# Q5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
#set.seed(33833)
modRF <- train(y ~ ., data=vowel.train, method="rf")
res <- predict(modRF,vowel.test)
varImp(modRF)



