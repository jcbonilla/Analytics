###SCRIPT FOR Generating Dummy Variables
url<-"https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/Week-03/Movies.csv"
movies<-read.csv(url)
str(movies) 


#Lets explore "genre"
movies$Genre    #displays all the values in "Genre"
unique(movies$Genre)     #lists all the unique values
table(movies$Genre)      #list & summarizes types & count of Genre


# Notice Genre values are treated as numbers. We can deal with this in two ways:
# 1. Change the data type for Genre to a factor variable
movies$Genre <- factor(movies$Genre)
str(movies)

# 2. Or create a new factor variable and assign it values that spell out the movies types in the dataset.
genrenames<-factor(movies$Genre, levels=1:4, labels=c("action", "comedy", "kids", "other"))
movies$Genre <- genrenames



# Create dummy variables for each of the movies categories
movies$Action <- as.logical(0)
movies$Comedy <- as.logical(0)
movies$Kids <- as.logical(0)
movies$Other <- as.logical(0)
str(movies)  #see how four new columns have been added at the end

#This is a for loop function.  
#This allows you assign a condition, that if met, will assign the parameters in "if"
#If NOT met, it assigns the parameters in "else"
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


View(movies)   
