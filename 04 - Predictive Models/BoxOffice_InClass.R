# Load the packages we will be using
library(ggplot2)

# Read the data file -- make sure you either provide the full path for the location of the file 
# or save your file in your working directory
url<-"https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/Week-03/Movies.csv"
movies<-read.csv(url)

# Examine the database
summary(movies)

# Notice Genre values are treated as numbers. We can deal with this in two ways:
# 1. Change the data type for Genre to a factor variable
movies$Genre <- factor(movies$Genre)

# 2. Or create a new factor variable and assign it values that spell out the movies types in the dataset.
genrenames<-factor(movies$Genre, levels=1:4, labels=c("action", "comedy", "kids", "other"))
movies$Genre <- genrenames

#Basic plotting
# Genre distribution
plot(movies$Genre)

# Revenue distribution
hist(movies$Opening_Week_Revenue)

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
movies_for_corr <- subset(movies, select=-(c(Movie,Genre)))
cor(movies_for_corr, use="complete.obs", method="kendall") 

# Linear Regression Model
revenue_model<- lm(Opening_Week_Revenue ~ Num_Theaters + Overall_Rating + Genre, data=movies)

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
new_movie <- data.frame(Movie="You Name It",Opening_Week_Revenue=NA,Num_Theaters=3611,Overall_Rating=57,
                        Action=TRUE,Comedy=FALSE,Kids=FALSE,Other=FALSE)

# Predict the opening week revenue for this new movie based on the linear regression model we built
new_movie$Predicted_Revenue <- predict(revenue_model3,new_movie)

# Output to csv file
write.csv(new_movie,file="~/GDrive/Business Analytics/BA Data/MoviePrediction.csv")

