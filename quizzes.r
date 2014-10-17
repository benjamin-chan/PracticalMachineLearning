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
# Make a plot of the outcome (CompressiveStrength) versus the index of the samples.
# Color by each of the variables in the data set (you may find the cut2() function in the Hmisc package useful for turning continuous covariates into factors).
# What do you notice in these plots?
training$index <- seq(1, nrow(training))
require(reshape2)
D <- melt(training, id.var=c("index"))
ggplot(D, aes(x=index, y=value, color=variable)) +
  geom_point(alpha=1/2) +
  geom_smooth(alpha=1/2) +
  facet_wrap(~ variable, nrow=3, scales="free_y") +
  theme(legend.position="none")
ggplot(training, aes(x=Cement, y=CompressiveStrength)) +
  geom_point(alpha=1/2) +
  geom_smooth(alpha=1/2) +
  geom_rug(alpha=1/4)
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
