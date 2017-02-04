##########################################
###
### Classification code
###  
###  Created:J.C. Bonilla; jb3379@nyu.edu
###
##########################################


# Load required packages
library(rpart)
library(rpart.plot)
library(ggplot2)
url<-"https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/bank_marketing.csv"
bank<-read.csv(url, header=TRUE,stringsAsFactors=FALSE)



#Explore the data 
str(bank)

# Plot the breakdown of subscribers to non-susbcribers
ggplot(data=bank, aes(x=subscribe)) + 
  geom_bar(alpha = 0.6, fill = "blue") + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Subscribed?", y = "Count")

#Distribution of Last Contact Duration
ggplot(bank, aes(x=duration/60))+ geom_histogram (alpha = 0.6, fill = "blue", breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=12)) +
  labs(x= "Last Contact Duration, mins", y = "Count")

# Distribution of previous campaign outcome
ggplot(data=bank, aes(x=poutcome)) + 
  geom_bar(alpha = 0.6, fill = "blue") + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Previous Campaign Outcome", y = "Count")

# Subscription status by previous outcome
ggplot(data=bank, aes(x=subscribe, fill = poutcome)) + 
  geom_bar(alpha = 0.6) + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Subscribed?", y = "Count")

# Subscription status by education
ggplot(data=bank, aes(x=subscribe, fill = education)) + 
  geom_bar(alpha = 0.6) + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Subscribed?", y = "Count")

# Look at more drilled down summaries specifically for our target variable 
# This is one way of looking at frequencies
count(bank,c("subscribe","marital"))


# Classification Tree with CART modeling for determining if the client will subcribe for a term 
# deposit or not 
decision_tree <- rpart(subscribe ~ age + job + marital + education + default + balance + housing +
                         loan + contact + day + month + duration + campaign + pdays + previous +
                         poutcome, method="class", data=bank)

# Detailed summary of the resulting decision tree
summary(decision_tree)

# Display the performance results
printcp(decision_tree)

# Visualize cross-validation results
plotcp(decision_tree)

# Output of the decision tree
plot(decision_tree, uniform=TRUE, main="Classification Tree")
text(decision_tree, use.n=TRUE, all=TRUE, cex=.8)

# Create plot of tree using the rpart.plot package for a better look
rpart.plot(decision_tree)


