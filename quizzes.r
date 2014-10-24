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



# Quiz 4
# Question 1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
table(vowel.train$y)
set.seed(33833)
require(caret)
M1 <- train(y ~ ., data=vowel.train, method="rf")
M2 <- train(y ~ ., data=vowel.train, method="gbm")
hat1 <- predict(M1, vowel.test)
hat2 <- predict(M2, vowel.test)
confusionMatrix(hat1, vowel.test$y)$overall
confusionMatrix(hat2, vowel.test$y)$overall
hat <- data.frame(hat1,
                  hat2,
                  y = vowel.test$y,
                  agree = hat1 == hat2)
accuracy <- sum(hat1[hat$agree] == hat$y[hat$agree]) / sum(hat$agree)
accuracy
# Question 2
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
M1 <- train(diagnosis ~ ., data=training, method="rf")
M2 <- train(diagnosis ~ ., data=training, method="gbm")
M3 <- train(diagnosis ~ ., data=training, method="lda")
hat1 <- predict(M1, testing)
hat2 <- predict(M2, testing)
hat3 <- predict(M3, testing)
hat <- data.frame(hat1, hat2, hat3, diagnosis=testing$diagnosis)
M4 <- train(diagnosis ~ ., data=hat, method="rf")
M4
hat4 <- predict(M4, testing)
confusionMatrix(hat1, testing$diagnosis)$overall
confusionMatrix(hat2, testing$diagnosis)$overall
confusionMatrix(hat3, testing$diagnosis)$overall
confusionMatrix(hat4, testing$diagnosis)$overall
# Question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
M1 <- train(CompressiveStrength ~ ., data=training, method="lasso")
M1
plot(M1$finalModel, xvar="penalty")
# Question 4
library(lubridate)  # For year() function below
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
dat = read.csv(url)
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
require(forecast)
M <- bats(tstrain)
M
hat <- forecast(M, length(testing$visitsTumblr))
hat <- cbind(testing, data.frame(hat))
hat$isIn95 <- hat$Lo.95 < hat$visitsTumblr & hat$visitsTumblr < hat$Hi.95
prop.table(table(hat$isIn95))
#  Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
require(e1071)
M <- svm(CompressiveStrength ~ ., data=training)
testing$hat <- predict(M, testing)
testing$error <- testing$CompressiveStrength - testing$hat
rmse <- sqrt(mean(testing$error ^ 2))
