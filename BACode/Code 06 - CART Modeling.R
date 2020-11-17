### Business Analytics at NYU
### Author: JC Bonilla
### jb3379@nyu.edu

### Decision Threes & CART Modeling via rpart

# Classification and regression trees can be calculated with 
# the *rpart* function in the library(rpart)    

# Business  Problem:  There’s a common scam amongst motorists 
# where a person will slam on his breaks in heavy traffic with
# the intention of being rear-ended. The person will then file 
# an insurance claim for personal injury and damage to his vehicle,
# alleging that the other driver was at fault. 
# Suppose we want to predict which of an insurance company’s 
# claims are fraudulent using a decision tree.  


#To start, we need to build a training set of known fraudulent claims.

train <- data.frame(ClaimID = c(1,2,3),
                    RearEnd = c(TRUE, FALSE, TRUE),
                    Fraud = c(TRUE, FALSE, TRUE))
train



library(rpart) #load the rpart package
library(rpart.plot) #package to visualize decision trees
mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class")
mytree


#Notice the output shows only a root node. 
# This is because rpart has some default parameters 
# that prevented our tree from growing. Namely: minsplit and minbucket. 
# minsplit is “the minimum number of observations that must exist in a node 
# in order for a split to be attempted”. 
# minbucket is “the minimum number of observations in any terminal node”. 

# We can plot mytree by loading the rattle package (and some helper packages) 
# and using the fancyRpartPlot() function.
rpart.plot(mytree)  # one node only

#See what happens when we override these parameters.
mytree.2 <- rpart(Fraud ~ RearEnd, data = train, method = "class",
                minsplit = 2, minbucket = 1)
mytree.2

#Now our tree has a root node, one split and two leaves (terminal nodes). 
# Observe that rpart encoded our boolean variable as an integer (False = 0, True = 1). 

rpart.plot(mytree.2)


# Example 2. In this example we explore a CART model for personal credit (good vs bad)
credit <-read.csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/credit.csv")
str(credit)

#split
set.seed(1234)
split<-(2/3)
trainingRowIndex <- sample(1:nrow(credit),(split)*nrow(credit))  # row indices for training data
trainingData <- credit[trainingRowIndex, ]  # model training data
testData  <- credit[-trainingRowIndex, ]   # test data

#model
creditTree<-rpart(Credit~CreditAmount + Age + CreditHistory + Employment, 
                  data = trainingData, method = "class")
creditTree
printcp(creditTree)
rpart.plot(creditTree)

# Prediction
prediction <- predict(creditTree, testData, type = "class")

conf.table <- data.frame(actual= testData$Credit, Prediction = prediction)
table(conf.table)

#other ways to review accuracy
printcp(creditTree)  # print the cptable
mean(testData$Credit != prediction)    # % misclassification error 
summary(creditTree)

# To plot the ROC we need the library(pROC)
library(pROC)
prediction.prob <- predict(creditTree, testData, type = "prob")
plot(roc(testData$Credit,prediction.prob[,2]))
auc(testData$Credit,prediction.prob[,2])
