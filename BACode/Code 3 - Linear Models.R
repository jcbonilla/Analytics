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

str(mydata)
dim(mydata)
summary(mydata)
sapply(mydata, sd)  #applies function sd() over mydata 
mylogit <- glm(admit ~ gre + gpa + rank, 
               data = mydata, 
               family = "binomial")
mylogit
summary(mylogit)
confint(mylogit)

