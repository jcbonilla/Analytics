### Business Analytics at NYU
### Author: JC Bonilla
### jb3379@nyu.edu


library(titanic)
library(rpart)
library(caret)
library(rpart.plot)
library(RColorBrewer)


str(titanic_train)

# Need a split train/test for model validation
trainingRowIndex <- sample(1:nrow(titanic_train),(.8)*nrow(titanic_train))  # row indices for training data

train <- titanic_train[trainingRowIndex, ]  # model training data
test  <- titanic_train[-trainingRowIndex, ]   # test data
# Recreate the gender model
fit <- rpart(Survived ~ Sex, data=train, method="class")
rpart.plot(fit)

# Build a deeper tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
             data=train, method="class")
# Plotting the tree
rpart.plot(fit, type =2, tweak = 2)

# Now let's make a prediction and get evaluate the model
Prediction <- predict(fit, test, type = "class")
table(Prediction, test$Survived)
require(caret)
confusionMatrix(Prediction, as.factor(test$Survived))


# Manually trim a decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0.005))
rpart.plot(fit, type =2, tweak = 2)

# Now let's make a prediction and compare models
Prediction2 <- predict(fit, test, type = "class")
table(Prediction, test$Survived)
table(Prediction2, test$Survived)
confusionMatrix(Prediction, as.factor(test$Survived))


# one more trick

new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

# Let's unleash the decision tree and let it grow to the max
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
rpart.plot(fit)

