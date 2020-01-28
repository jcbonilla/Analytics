### Business Analytics Course, New York University
### Author: JC Bonilla
### jb3379@nyu.edu


#Loading data
url<-"https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/zagat.CSV"
data<-read.csv(url, header=TRUE,stringsAsFactors=FALSE)


dim(data)
str(data)
names(data)

#Ploting options
hist(data$Price)

boxplot(data$Price, col="blue")
boxplot(data$Decor, col="red")
boxplot(data$Decor, col="orange", outline= FALSE)  # removes outliers


plot(data$Price, data$Food)
plot(data)
boxplot(data$Food, data$Decor, data$Service, data$Price, col="blue")

#For multiple plots
par(mfrow=c(2,2)) 
hist(data$Food)
hist(data$Decor, col="blue")
hist(data$Service, col="green")
hist(data$Price, col="red")

zagat<-data


#plotting histograms

par(mfrow=c(2,2)) 
hist((zagat$Service), col="blue",border="white")
hist((zagat.z3$Service), col="red",border="white")
hist((zagat.z2$Service), col="red",border="white")
hist((zagat.z1$Service), col="red",border="white")

par(mfrow=c(4,1)) 
hist((zagat$Service), col="blue",border="white")
hist((zagat.z3$Service), col="red",border="white")
hist((zagat.z2$Service), col="red",border="white")
hist((zagat.z1$Service), col="red",border="white")



# Calibrating plots 
par(mfrow=c(4,1))  
hist((zagat$Service), col="blue",border="white", breaks=20, xlim=c(5,25))
hist((zagat.z3$Service), col="red",border="white", breaks=20, xlim=c(5,25))
hist((zagat.z2$Service), col="red",border="white", breaks=20, xlim=c(5,25))
hist((zagat.z1$Service), col="red",border="white", breaks=20, xlim=c(5,25))

#plotting box plots
par(mfrow=c(4,1)) 
boxplot(zagat$Service, horizontal = TRUE, col="yellow")
boxplot(zagat.z3$Service, horizontal = TRUE, col="red")
boxplot(zagat.z2$Service, horizontal = TRUE, col="green")
boxplot(zagat.z1$Service, horizontal = TRUE, col="blue")

boxplot(zagat$Service, horizontal = TRUE, col="yellow")
boxplot(zagat.z3$Service, horizontal = TRUE, col="red")
boxplot(zagat.z2$Service, horizontal = TRUE, col="green")
boxplot(zagat.z1$Service, horizontal = TRUE, col="blue",
        xlab="Service", ylab="Zagat")
