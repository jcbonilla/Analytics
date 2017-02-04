##########################################
###
### Simple Linear Regression
###  
###  Created:J.C. Bonilla; jb3379@nyu.edu
###
##########################################


data <- read.csv("~/Google Drive/NYU GDrive/Teaching/Business Analytics/BA Data/Sales vs Advertisement.csv",        
                 header=TRUE, stringsAsFactors=TRUE)
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

par(mfrow=c(1,1))
plot(x,y , col="red", lwd=3,
     ylab="Advertisement", xlab="Sales per month")
abline(model)

par(mfrow=c(2,2))
plot(model)
