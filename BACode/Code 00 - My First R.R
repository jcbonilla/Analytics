### Data Analytics - MBA@RICE
### Author: JC Bonilla
### jb132@rice.edu
### 

### This 1st script include 3 parts: 
              # 1. Load Data
              # 2. Exploring Data
              # 3. Analyzing Data


##########################
####### Load Data ########
##########################

#Methods 3: loading data from GitHub
url<-"https://raw.githubusercontent.com/jcbonilla/MBA-RICE/master/Data/Casino.csv"
casino<-read.csv(url, header=TRUE,stringsAsFactors=TRUE)
casino

###############################
####### Exploring Data ########
###############################

casino  # generates an "indexed" view in R
View(casino)  # geneates an "spreadsheet" view in R
str(casino) # summarizes structure of data
head(casino,10) # summaries of top 10 records
names(casino)  #displays headers


###############################
####### Analyzing Data ########
###############################

summary(casino)   #summary stats on all variables
boxplot(casino)   #box plot
boxplot(casino, horizontal = T, notch=T, col="blue", outline = F)

#lets drill-down on Total.Spend
casino$Total.Spend  # to invoke a column
mean(casino$Total.Spend)  # mean 
sd(casino$Total.Spend) # standard deviation

#boxplots
boxplot(casino$Total.Spend, col="red", horizontal = T)
boxplot(casino$Total.Spend~casino$Gender, col="red", horizontal = T)
boxplot(casino$Total.Spend~casino$Gender, col="red", horizontal = T, outline=F)

#histograms
hist(casino$Total.Spend, col="blue")
hist(casino$Total.Spend, col="blue", breaks = 50)

