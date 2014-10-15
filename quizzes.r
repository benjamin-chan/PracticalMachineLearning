# Quiz 2
# Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
# Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
ggplot(training, aes(x=trainIndex, y=CompressiveStrength)) +
  geom_line()
# Question 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(Superplasticizer, data=training, geom="histogram")
table(training$Superplasticizer)
# Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
predVar <- grep("^IL", names(training))
str(training[predVar])
preProcess(training[predVar], method="pca", thresh=0.8)
# Question 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
predVar <- grep("^IL", names(training))
M0 <- train(training$diagnosis ~ ., data=training[predVar], method="glm")
hat0 <- predict(M0, testing)
confusionMatrix(testing$diagnosis, hat0)
preProc <- preProcess(training[predVar], method="pca", thresh=0.8)
trainPC <- predict(preProc, training[predVar])
M1 <- train(training$diagnosis ~ ., data=trainPC, method="glm")
testPC <- predict(preProc, testing[predVar])
hat1 <- predict(M1, testPC)
confusionMatrix(testing$diagnosis, hat1)
