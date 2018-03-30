### Business Analytics at NYU
### Author: JC Bonilla
### jb3379@nyu.edu



url <-"https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/bank_marketing%20-%20imputation.csv"
bankdata<-read.csv(url, header = TRUE)
#data at https://archive.ics.uci.edu/ml/datasets/Bank+Marketing 

str(bankdata)
bankdata.raw<-bankdata
#  reduced dataset for class
library(caret)
index <- createDataPartition(bankdata$y, p=0.1, list=FALSE)
bankdata.x <- bankdata[ index,] 

bankdata<-bankdata.x   # see reduction script

##########################
##1.  DATA PREP - MUNGING
##########################
# check for missing values & data quality
sum(is.na(bankdata))

library(dataQualityR)
setwd("~/Downloads")
checkDataQuality(data = bankdata, 
                 out.file.num ="dq_num.csv", 
                 out.file.cat= "dq_cat.csv")
dq_num<-read.csv("~/Downloads/dq_num.csv")
dq_cat<-read.csv("~/Downloads/dq_cat.csv")
View(dq_num)
View(dq_cat)


# Imputation - CARET permits unputation on numerical vairables. 
# It predicts missing values based on other attributes for that row.
summary(bankdata$age)
summary(bankdata$marital)

preProcValues <- preProcess(bankdata, method = c("medianImpute"))
bankdata_processed <- predict(preProcValues, bankdata)
sum(is.na(bankdata_processed)) # check for missing values

summary(bankdata_processed$age)  # compare imputation results
summary(bankdata_processed$marital)   # this requires a prediction.  Let ignore for now

# Dummnification -  First, convert the dependent variable to numerical.
summary(bankdata_processed$y)

#Converting every categorical variable to numerical using dummy variables
bankdata_processed$y<- as.integer(ifelse(bankdata_processed$y=="yes","1","0"))
table(bankdata_processed$y)

dmy <- dummyVars(" ~ .", data = bankdata_processed,fullRank = T)
bankdata_dmy <- data.frame(predict(dmy, newdata = bankdata_processed))

#Checking the structure of transformed train file
summary(bankdata_dmy$y)
bankdata_dmy$y<- as.factor(bankdata_dmy$y)  #convert outcome variable to numeric

###############
## 2. MODELING
###############

names(getModelInfo())  #200+ ML algorithms

modelLookup(model='rf')  # To find the parameters of a model that can be tuned
modelLookup(model='gbm') 


outcomeName <- 'y'
predictorNames <- names(bankdata_dmy)[names(bankdata_dmy) != outcomeName]
bankdata_dmy$y<- ifelse(bankdata_dmy[,outcomeName]=="1","subscribed","nope")
bankdata_dmy$y<-as.factor(bankdata_dmy$y)


#Spliting training set into two parts based on outcome: 75% and 25%
set.seed(1234)  # setting seed to reproduce results of random sampling

index <- createDataPartition(bankdata_dmy$y, p=0.75, list=FALSE)
train <- bankdata_dmy[ index,] #  training data
test <- bankdata_dmy[-index,]  # test data

# class imbalance check
prop.table(table(train$y))  # will incorporate over- and under-sampling techniques with cross-validation resampling. 

fitControl <- trainControl(method = "repeatedcv",   #  5-Fold cross-validation repeated 5 times.
                           number = 2,
                           repeats = 2,
                           sampling = "down")  # see https://shiring.github.io/machine_learning/2017/04/02/unbalanced

model_rf<-train(train[,predictorNames],train[,outcomeName],
                method='rf',
                trControl=fitControl)

model_gbm<-train(train[,predictorNames],train[,outcomeName],
                 method='gbm',
                 trControl=fitControl)

# summarizing the models
print(model_rf)
plot(model_rf)

print(model_gbm)
plot(model_gbm)

#Creating grid
grid <- expand.grid(n.trees=c(10,20),
                    shrinkage=c(0.01,0.05),
                    n.minobsinnode = c(3,5),
                    interaction.depth=c(1,5))


# training the model
model_gbm_tuned<-train(train[,predictorNames],train[,outcomeName],
                       method='gbm',
                       trControl=fitControl,
                       tuneGrid=grid)


print(model_gbm_tuned)
plot(model_gbm_tuned)

# adv. model tuning - https://rpubs.com/chengjiun/52658 


#Checking variable importance
varImp(object=model_rf)
plot(varImp(object=model_rf),main="RandomForest - Variable Importance")

varImp(object=model_gbm)
plot(varImp(object=model_gbm),main="GBM - Variable Importance")

varImp(object=model_gbm_tuned)
plot(varImp(object=model_gbm_tuned),main="Tuned GBM - Variable Importance")


##################
## 3. PREDICTION
##################
predictions.gbm<-predict.train(object=model_gbm,test[,predictorNames],type="raw")
table(predictions.gbm)

predictions.gbm.tuned<-predict.train(object=model_gbm_tuned,test[,predictorNames],type="raw")
table(predictions.gbm.tuned)

predictions.rf<-predict.train(object=model_rf,test[,predictorNames],type="raw")
table(predictions.rf)

confusionMatrix(predictions.rf,test[,outcomeName])
confusionMatrix(predictions.gbm,test[,outcomeName])
confusionMatrix(predictions.gbm.tuned,test[,outcomeName])



##############
## OTHER
#############


#Feature selection using Caret
# we’ll be using Recursive Feature elimination which is a wrapper method 
# to find the best subset of features to use for modeling. 
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)

bankdata_Profile <- rfe(train[,predictorNames], train[,outcomeName],
                        rfeControl = control)  #this is going to take 20-30min
bankdata_Profile
# RFE finds the best performing feature subset by. 
# repeatedly creating models and keeps aside the best or the worst performing feature 
# at each iteration. It constructs the next model with the left features until
# all the features are exhausted. It then ranks the features based on the order 
# of their elimination.
