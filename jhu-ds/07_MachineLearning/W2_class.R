library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type ~ .,  data=training, method="glm")
modelFit

modelFit$finalModel

predictions <- predict(modelFit, newdata=testing)
predictions

confusionMatrix(predictions, testing$type)

set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = TRUE)
sapply(folds, length)
folds[[1]][1:10]

folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds, length)
folds[[1]][1:10]

tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$test[[1]]


args(train.default)


## class #2
library(ISLR); library(ggplot2); library(caret)
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)


featurePlot(x=training[,c("age", "education", "jobclass")],
            y=training$wage, plot="pairs")

qplot(age, wage, data=training)

qq <- qplot(age, wage, colour=education, data=training)
qq + geom_smooth(method='lm', formula=y~x)

library(Hmisc)
cutWage <- cut2(training$wage, g=3)
table(cutWage)

p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"))
p1

p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot", "jitter"))
p2
grid.arrange(p1, p2, ncol=2)

t1 <- table(cutWage, training$jobclass)
t1

prop.table(t1,1)

qplot(wage, colour=education, data=training, geom="density")

# new class 

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main="", xlab="ave. capital run length")

mean(training$capitalAve)
sd(training$capitalAve)

# Standardizing
trainCapAve <- training$capitalAve
trainCapAves <- (trainCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAves)
sd(trainCapAves)

# we have to use same parameter in training set to standard test set
testCapAve <- testing$capitalAve
testCapAves <- (testCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(testCapAves)
sd(testCapAves)

# use preProcess function,[,-58] remove the label column
preObj <- preProcess(training[,-58], method=c("center","scale"))
trainCapAves <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAves)
sd(trainCapAves)

testCapAves <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAves)
sd(testCapAves)

# combine preProcess into train
set.seed(32343)
modelFit <- train(type~., data=training, 
                  preProcess=c("center", "scale"), method="glm")
modelFit

# Box-Cox transforms
preobj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAves <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAves); qqnorm(trainCapAves)

# Imputing data
set.seed(13343)

training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

# Standardize tru values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve-capAveTruth)
quantile((capAve-capAveTruth)[selectNA])
quantile((capAve-capAveTruth)[!selectNA])

# class
library(kernlab); data(spam)
spam$capitalAveSq <- spam$capitalAve^2

library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain, ]

table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

library(splines)
bsBasis <- bs(training$age,df=3)
bsBasis

lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)


# spline on test set: 
predict(bsBasis, age=testing$age)


# class

library(caret); library(kernlab); data(spam);
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]; testing <- spam[-inTrain, ]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M>0.8,arr.ind=T)

names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])

prComp$rotation

typeColor <- ((spam$type=="spam")*1+1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")


preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp = 2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp = 2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type ~ ., method="glm", data=trainPC)

testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit,testPC))

modelFit <- train(training$type ~ ., method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type, predict(modelFit,testing))

# class
library(caret); data(faithful); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)

plot(trainFaith$waiting, trainFaith$eruptions, pch=19, 
     col="blue", xlab="Waiting", ylab="Duration")

lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)

plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue")
lines(trainFaith$waiting, lm1$fitted.values, lwd=3)

newdata <- data.frame(waiting=80)
predict(lm1, newdata)

par(mfrow=c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, 
     col="blue", xlab="Waiting", ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting, testFaith$eruptions, pch=19, 
     col="blue", xlab="Waiting", ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

# calculate RMSE
sqrt(sum((lm1$fitted.values-trainFaith$eruptions)^2))
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))

# prediction intervals
pred1 <- predict(lm1, newdata=testFaith, interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting[ord], pred1[ord,], type="l", col=c(1,2,2),
        lty=c(1,1,1), lwd= 3) 

modFit <- train(eruptions ~ waiting, data=trainFaith, method="lm")
summary(modFit$finalModel)

# class
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage, select=-c(logwage))
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage, plot="pairs")
qplot(age, wage, data=training, colour=jobclass)
qplot(age, wage, data=training, colour=education)

modFit <- train(wage ~ age+jobclass+education, 
                method="lm", data=training)
finMod <- modFit$finalModel
print(modFit)

plot(finMod, 1, pch=19, cex=0.5, col="#00000010")
qplot(finMod$fitted.values, finMod$residuals, colour=race, data=training)
# plot by index
plot(finMod$residuals, pch=19)

# predicted vs. truth in test set
pred <- predict(modFit, testing)
gplot(wage, pred, colour=year, data=testing)

# use all covariates
modFitAll <- train(wage ~ ., data=training, method="lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, data=testing)

