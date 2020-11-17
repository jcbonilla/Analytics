### Business Analytics at NYU
### Author: JC Bonilla
### jb3379@nyu.edu

#### BOX OFFICE###

# Load the packages we will be using
library(ggplot2)
library(dataQualityR)
library(Hmisc)
library (reshape2)

# Read the data file
url<-"https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/Movies.csv"
movies<-read.csv(url)

# Examine the database & data audit
str(movies)
View(movies)

# DATA EXPLORATION & DESCRIPTIVE STATS
# Notice Genre values are treated as numbers. We can deal with this in two ways:
# 1. Change the data type for Genre to a factor variable
movies$Genre <- factor(movies$Genre)

# 2. Or create a new factor variable 
# and assign it values that spell out the movies types in the dataset.
genrenames<-factor(movies$Genre, levels=1:4, labels=c("action", "comedy", "kids", "other"))
movies$Genre <- genrenames

# Lets review quality of the data
checkDataQuality(data=movies, 
                 out.file.num ="~/Downloads/dq_movies_num.csv", 
                 out.file.cat= "~/Downloads/dq_movies_cat.csv")
dq_num<-read.csv("~/Downloads/dq_movies_num.csv")
View(dq_num)

dq_cat<-read.csv("~/Downloads/dq_movies_cat.csv")
View(dq_cat)


# Plot the distribution of movies by type in our data
ggplot(data=movies, aes(x=Genre)) + 
  geom_bar(alpha = 0.6, fill = "blue") + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Genre", y = "Number of Movies")

# Plot revenue distribution of the movies
ggplot(movies, aes(x=Opening_Week_Revenue))+ geom_histogram (alpha = 0.6, fill = "blue") + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=12)) + 
  labs(x= "Opening Week Revenue, Millions $", y = "Number of Movies")

# Plotting opening week revenue by genre
ggplot(movies, aes(Genre, Opening_Week_Revenue)) +
  geom_boxplot(alpha = 0.6, fill = "blue", 
               outlier.colour = "blue", outlier.size = 3) + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=12)) + 
  labs(x= "Genre", y = "Opening Week Revenue, Millions $")

# Plot revenue against the number of theaters the movies is released at
ggplot(data=movies, aes(x=Num_Theaters, y=Opening_Week_Revenue)) + 
  geom_point(alpha = 0.6, colour="blue", size=3) + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Number of Theaters", y = "Opening Week Revenue, Millions $")

# Plot revenue against movie rating
ggplot(data=movies, aes(x=Overall_Rating, y=Opening_Week_Revenue)) + 
  geom_point(aes(colour=Genre), size=3) + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Rating", y = "Opening Week Revenue, Millions $")

# Correlation between two variables
cor(movies$Opening_Week_Revenue, movies$Overall_Rating)

# If you want to see the correlation along with the confidence interval:
cor.test(movies$Opening_Week_Revenue, movies$Overall_Rating)

# Correlation matrix
movies_for_corr <- subset(movies, select=-(c(Movie,Genre))) # remove categorical variables

cormatrix <- rcorr(as.matrix(movies_for_corr), type='spearman') # rcorr needs a matrix class as input
cormatrix

# Now we need to do prep the cormatrix for visualization.  
# With the function melt() we convert an object into a molten data frame
cordata = melt(cormatrix$r)  # cormatrix$r contains the values of the correlation matrix
head(cordata)

ggplot(data= cordata, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()+xlab("") +ylab("")

# Let's add some more formatting
ggplot(data= cordata, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white")+xlab("") +ylab("")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                     midpoint = 0, limit = c(-1,1), space = "Lab", 
                     name="Correlation\nCoeficient") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()

# Linear Regression Model
revenue_model<- lm(Opening_Week_Revenue ~ Num_Theaters + Overall_Rating + Genre,data=movies)
summary(revenue_model)  

# Create dummy variables for each of the movies categories
movies$Action <- as.logical(0)
movies$Comedy <- as.logical(0)
movies$Kids <- as.logical(0)
movies$Other <- as.logical(0)

for (i in 1:nrow(movies)){
  if (movies$Genre[i]=="comedy")
    movies$Comedy[i]<-as.logical(1)
  else if (movies$Genre[i]=="kids")
    movies$Kids[i]<-as.logical(1)
  else if (movies$Genre[i]=="other")
    movies$Other[i]<-as.logical(1)
  else
    movies$Action[i]<-as.logical(1)
}

# Checking the effect of dummy variables
# Model leaving out "Action"
revenue_model2<- lm(Opening_Week_Revenue ~ Comedy + Kids + Other, data=movies)
summary(revenue_model2) 

# Compare to mean value for Action
action <- subset(movies,Action==1)
mean(action$Opening_Week_Revenue)

comedy <- subset(movies,Comedy==1)
mean(comedy$Opening_Week_Revenue)

# Iteration of the Linear Regression Model with dummy variables
revenue_model3 <- lm(Opening_Week_Revenue ~ Num_Theaters + Overall_Rating + Comedy + Kids + Other, data=movies)

summary(revenue_model3) 

# Prediction on new data
# Create a data frame with the inputs for the new movie
new_movie <- data.frame(Movie="AWESOME",Opening_Week_Revenue=NA,Num_Theaters=3611,Overall_Rating=57,
                        Action=TRUE,Comedy=FALSE,Kids=FALSE,Other=FALSE)

# Predict the opening week revenue for this new movie based on the linear regression model we built
new_movie$Predicted_Revenue <- predict(revenue_model3,new_movie)
new_movie
