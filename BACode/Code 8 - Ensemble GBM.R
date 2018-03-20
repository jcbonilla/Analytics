### Business Analytics at NYU
### Author: JC Bonilla
### jb3379@nyu.edu


## ENSEMBLES
setwd("~/Downloads")
url<- "https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/school.csv"

school <- read.csv(url, header = TRUE, stringsAsFactors = TRUE)

str(school)
table(school$Status)
prop.table(table(school$Status))

# review the "balance" of the positive class we are about to predict
school$APPLICANT <- as.logical(0)
school$PROSPECT <- as.logical(0)

for(i in 1:nrow(school)) {
  if (school$Status[i]=="APPLICANT")
    school$APPLICANT[i] <- as.logical(1)
  else
    school$PROSPECT[i] <- as.logical(1)
}
# using bins for numerical variables
str(school)
summary(school$GPA)
summary(school$Family.Income)
summary(school$Distance.to.Campus.miles.)

school$GPAband <- cut(school$GPA, 10)
summary(school$GPAband)
school$IncomeBand <- cut(school$Family.Income, 20)
summary(school$IncomeBand)
school$DistanceBand <- cut(school$Distance.to.Campus.miles., 20)
summary(school$DistanceBand)


### Selecting final variables for modeling.  A total of 12 variables
library(dplyr)
names(school)
school.df <- select(school, 
                  Country,
                  State,
                  Gender,
                  Source,
                  InState,
                  Zip,
                  Race,
                  Religion,
                  GPA=GPAband,
                  Income=IncomeBand,
                  Distance = DistanceBand,
                  APPLICANT)

str(school.df)


# Now, we dummified all categorical variables

library(caret)
school.df$APPLICANT <- as.integer(school.df$APPLICANT)
schoolDummy <- dummyVars("~.",data=school.df, fullRank=F)
school.dummified <- as.data.frame(predict(schoolDummy,school.df))
school.dummified$APPLICANT <- as.factor(school.dummified$APPLICANT)
school.raw.dummy<-write.csv(school.dummified,file="school.dummified.csv")

# MODELING
outcomeName <- 'APPLICANT'
predictorsNames <- names(school.dummified)[names(school.dummified) != outcomeName]
school.dummified$APPLICANT<- ifelse(school.dummified[,outcomeName]=="1","apply","nope")
school.dummified$APPLICANT<-as.factor(school.dummified$APPLICANT)

set.seed(1234)  # setting seed to reproduce results of random sampling
split<-(.70)

trainingRowIndex <- sample(1:nrow(school.dummified),(split)*nrow(school.dummified))  # row indices for training data

train.df <- school.dummified[trainingRowIndex, ]  # model training data
test.df  <- school.dummified[-trainingRowIndex, ]   # test data

library(gbm)
# gbm setup
objControl <- trainControl(method='cv', number=3, returnResamp='none',
                           summaryFunction = twoClassSummary, classProbs = TRUE)


### Gbm Model
t1<-Sys.time()
objModel <- train(train.df[,predictorsNames], train.df[,outcomeName], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

t2<-Sys.time()
gbm.t<-t2-t1
gbm.t

gbmImp<-varImp(objModel, value = "nsubsets", scale=TRUE)
gbmImp
gbmImp.T<-as.data.frame(gbmImp$importance)
plot(gbmImp, top=20)
write.csv(gbmImp.T, "schoolfactors.csv")

predictions <- predict(object=objModel, test.df[,predictorsNames], type='raw')

actuals<-test.df$APPLICANT
table(predictions, actuals)
confusionMatrix(predictions, actuals)
predictions.p <- predict(object=objModel, test.df[,predictorsNames], type='prob')
test.prob <- cbind(test.df, predictions.p)
bands <- cut(test.prob$apply, breaks=c(0, 0.2,0.5,  1),
             levels=c(1, 2, 3, 4), include.lowest=TRUE)
scorebands <- cbind(test.prob, bands)
testbands<-aggregate(apply ~ bands, data = scorebands, length)
testbands

save(objModel, file="GBMmodel.Rdata1")

