### Business Analytics at NYU
### Author: JC Bonilla
### jb3379@nyu.edu

####SIMPLE LINEAR REGRESSION ###
url <- "https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/Sales%20vs%20Advertisement.csv" 
data <- read.csv(url,header=TRUE, stringsAsFactors=TRUE)
dim(data)
names(data)
x=data$Sales
y=data$Advertising.Dollars
par(mfrow=c(1,2))
plot(x,y , col="red", lwd=3,
     ylab="Advertisement", xlab="Sales per month")
plot(x,y, type="b", col="blue", lwd=3,
     ylab="Advertisement", xlab="Sales per month")

model<-lm(y ~ x)
model
summary(model) 


par(mfrow=c(2,2))
plot(model)


###GENERALIZED LINEAR MODELS ####

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")


head(mydata)
dim(mydata)
str(mydata)
View(mydata)
summary(mydata)
sapply(mydata, sd)  #applies function sd() over mydata 

# MODELING
mylogit <- glm(admit ~ gre + gpa + rank, 
               data = mydata, 
               family = "binomial")

summary(mylogit)

# PREDICTION
new_student<- data.frame(gre=600, gpa=3.4, rank=2)
predict(mylogit, new_student, type="response")
 
# MODELING with a training:testing split
# Step 1: Create the training and test (validation) data samples from original data.
set.seed(100)  # setting seed to reproduce results of random sampling
split<-(.8)
trainingRowIndex <- sample(1:nrow(mydata),(split)*nrow(mydata))  # row indices for training data
trainingData <- mydata[trainingRowIndex, ]  # model training data
testData  <- mydata[-trainingRowIndex, ]   # test data


#Step 2: Develop the model on the training data and use it to predict on test data

# Model
model<-{admit ~ gre + gpa + rank}
admit.lm <- glm(model, data=trainingData, family = binomial(link = "logit"))  # build the model

# Review diagnostic measures
summary(admit.lm)

# Step 3: Calculate prediction accuracy and error rates
response<- ifelse(predict(admit.lm, testData, type = "response")>.5, 1, 0)  # predict distance


actuals_preds <- data.frame(cbind(actuals=testData$admit, predicted=response))  # make actuals_predicteds dataframe.
head(actuals_preds)

# simple correlation between  actuals vs predicted is an accuracy measure. 
#  a higher correlation accuracy impliessimilar directional movement
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

