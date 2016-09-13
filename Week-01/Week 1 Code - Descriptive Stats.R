### Business Analytics Course, New York University
### Author: JC Bonilla
### jb3379@nyu.edu
### Content:
###      1. Calculations
###      2. Variables
###      3. Comments
###      4. Help
###      5. Functions
###      6. Data Types
###      7. Loading Data
###      8. Basic Analysis
###      9. Filtering data



###########################
###### Calculations #######
###########################

# After RStudio is started code in the Console (Below) or Debugging tool (above) 

# Lets enter numbers and perform calculations.

1 + 2 

5 + 55

5/1.23

6*6

sqrt(16)

2:20


#So, R does scalar arithmetic returning the scalar value. 
#In actual fact, R returns a vector of length 1 - hence the [1] denoting first element of the vector

##########################
####### Variables ########
##########################
# We assign values to variables with the assignment operator "=" or "<-"
x = 1  
y <- 2
x
y

s<-6
p<-4
t<-s+p

##########################
####### Comments #########
##########################
#All text after the pound sign "#" within the same line is considered a comment.

11 + 11      # this is a comment 

##########################
########## Help ##########
##########################
#R provides extensive documentation. For example, entering ?c or help(c) at the prompt gives documentation of the function c in R. Please give it a try.
help(c)

##########################
####### Functions ########
##########################
#R functions are invoked by its name, then followed by the parenthesis, and zero or more arguments.
#The following apply the function c to combine three numeric values into a vector.
c(1, 2, 3) 

vector<-c(5,9,1,0)

series<-1:10

seq(1,9,by=2)

seq(8,20,length=6)

rep(0,100)

rep(1:3,6)



##########################
###### Data Types ########
##########################
#There are several  R data types that are of frequent occurrence in routine R calculations. 
# Example are: numeric, integer, complex, logical, character
class(x)
z<-"I love R"
class(z)
data <- c(1,2,3)
class(data)

data.1<-list(1,2,3)
class(data.1)
data
data.1

##########################
####### Load Data ########
##########################

# loading data from .csv file:

data<- read.csv("~/Google Drive/_NYU GDrive/Teaching/Business Analytics/BA Data/zagat.csv", header=TRUE,
                stringsAsFactors=FALSE)   #direct method
setwd("~/Google Drive/_NYU GDrive/Teaching/Business Analytics/BA Data")   #using working directory
data<- read.csv("zagat.csv", header=TRUE,stringsAsFactors=FALSE)
getwd()   # display active directory

#exploring file
names(data)
dim(data)
class(data)
data
data[1:4]    # brackets [ ] allow indexing, columns 1-4
data[1:10,1:3]   # displays 10 rows and 3 columns
data$Price   # displays valyes for column "Price"
data$Price[1:10]
price<-data$Price  # dollar symbol $ is used to invoce a vector in a matrix

# other ways to explore data imports 
head(data)
str(data)

##########################
#### Basic Analysis ######
##########################

#Basic Functions
  mean()
  var()
  sd()
  min()
  max()
  median()
  quantile()
  cor()


mean(data$Price)  
var(data$Price)  
sd(data$Price)  
min(data$Price)  
max(data$Price)  
median(data$Price)  
quantile(data$Price)  
cor(data$Price,data$Food)  
summary(data$Price)

##########################
#### Filtering data ######
##########################
data.1<-subset(data, Price == 50)
data.2<-subset(data, Price > 50)
dim(data.1)
dim(data.2)


###############################
#### Computing z-scores ######
##############################
zagat<-data
service.sd <- sd(zagat$Service)
service.mean <- mean(zagat$Service)
z <- (zagat$Service-service.mean)/service.sd

#creating subsets of data by Z value (3, 2, and 1 standard dev)
zagat.z3 <- subset(zagat, z<3)
zagat.z2 <- subset(zagat, z<2)
zagat.z1 <- subset(zagat, z<1)

# Computing same calculation for data without outliers
dim(zagat)
summary(zagat$Service)
esd(zagat$Service)

dim(zagat.z3)
summary(zagat.z3$Service)
sd(zagat.z3$Service)

dim(zagat.z2)
summary(zagat.z2$Service)
sd(zagat.z2$Service)

dim(zagat.z1)
summary(zagat.z1$Service)
sd(zagat.z1$Service)




