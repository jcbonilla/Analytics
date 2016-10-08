# Load required packages
library(rpart)
library(rpart.plot)
library(ggplot2)
library(plyr)
library(reshape2)

# Load data
url<-"https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/Week-05/bankloan.csv"
bank <- read.csv(url)
names(bank)
str(bank)

#Explore the data 
summary(bank)

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

# Cross tabulation with proportions
library(gmodels)
CrossTable(bank$subscribe, bank$marital, prop.r=TRUE, prop.c=TRUE,prop.t=FALSE,
           prop.chisq=FALSE)

CrossTable(bank$subscribe, bank$poutcome, prop.r=TRUE, prop.c=TRUE,prop.t=FALSE,
           prop.chisq=FALSE)


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

#Trying out different plotting options
# Just with the decision criteria at the nodes
rpart.plot(decision_tree, type = 1)
# Include the number of records classified as yes or no
rpart.plot(decision_tree, type = 2, extra=2)
# Probability per class of observations in the node (conditioned on the node, sum across a node is 1).
rpart.plot(decision_tree, type = 2, extra=4)
#The probabilities times the fraction of observations in the node (the probability relative to all observations, sum across all leaves is 1).
rpart.plot(decision_tree, type = 2, extra=9)

# Plot the confusion matrix
conf.matrix <- table(bank$subscribe, predict(decision_tree,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)


