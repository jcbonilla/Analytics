### Analytics at NYU
### Author: JC Bonilla
### jb3379@nyu.edu

####    PROJECT 2.   ####
#### Scoring Process ####
library(dplyr)
library (caret)

#simplified model 
data=read.csv("~/Downloads/trainingData.csv", stringsAsFactors = TRUE)
data$Y=as.factor(data$Y)
data.df = subset(data, select = -c(id, toCoupon_GEQ5min)) #drops from modeling

index = createDataPartition(data.df$Y, p=.80, list=FALSE) # row indices for training data



outcomeName='Y'
predictorNames <- names(data.df)[names(data.df) != outcomeName] 

train.df <- data.df[ index,]  # model training data
test.df<- data.df[ -index,]   # test data

fitControl <- trainControl(method = "none")   # control parameters for training
gbm.model<-train(train.df[,predictorNames],train.df[,outcomeName],
          method='gbm',
          trControl=fitControl)

# scoring submisions process
score.data=read.csv("~/Downloads/scoringData.csv", stringsAsFactors = TRUE). #load scoring data
score.df = subset(score.data, select = -c(id, toCoupon_GEQ5min)) #drops from modeling

scoring=predict(gbm.model,score.df[,predictorNames],type="raw"). #scoring
table(scoring)
submission.df=read.csv("~/Downloads/teamX_submission.csv", stringsAsFactors = TRUE) #load template
submission.df$Y=scoring #append submission template with scores
write.csv( submission.df, "~/Downloads/teamJC_submission.csv", row.names = FALSE) #creates .csv file for loading
