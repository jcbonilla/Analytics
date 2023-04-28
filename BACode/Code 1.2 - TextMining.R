### BA - Text as Data  
### Author: JC Bonilla
### jb3379@nyu.edu

library(tm)
library(quanteda)
#load .csv file with news articles
url<-"https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/NewsText.csv"

precorpus<- read.csv(url,header=TRUE, stringsAsFactors=FALSE)   #file load

dim(precorpus) # dim of file; 107 files with 11 headers
names(precorpus)   # names of the headers
head(precorpus)
str(precorpus)
head(precorpus$Full.text)

##########################
### PREPROCESSING
##########################

#clean up process using gsub and regular expressions
precorpus$Full.text <- gsub("'", "", precorpus$Full.text) # remove apostrophes
precorpus$Full.text <- gsub("[[:punct:]]", " ", precorpus$Full.text)  # replace punctuation with space
precorpus$Full.text <- gsub("[[:cntrl:]]", " ", precorpus$Full.text)  # replace control characters with space
precorpus$Full.text <- gsub("^[[:space:]]+", "", precorpus$Full.text) # remove whitespace at beginning of documents
precorpus$Full.text <- gsub("[[:space:]]+$", "", precorpus$Full.text) # remove whitespace at end of documents
precorpus$Full.text <- gsub("[^a-zA-Z -]", " ", precorpus$Full.text) # allows only letters
precorpus$Full.text <- tolower(precorpus$Full.text)  # force to lowercase
head(precorpus$Full.text)

#create a corpus
help(corpus)
names(precorpus)
newscorpus<- corpus(precorpus$Full.text)
#explore the corpus
names(newscorpus)
summary(newscorpus)  #summary of corpus

#create a corpus with metadata
newscorpus<- corpus(precorpus$Full.text,
                    docnames=precorpus$Document_ID,
                    docvar=data.frame(Year=precorpus$Publication.year,
                                      Subject= precorpus$Subject,
                                      Place=precorpus$Location,
                                      Title= precorpus$Title))
names(newscorpus) 
summary(newscorpus)  #summary of corpus

#create a corpus
help(corpus)
names(precorpus)
newscorpus<- corpus(precorpus$Full.text)
#explore the corpus
names(newscorpus)
summary(newscorpus)  #summary of corpus

#create a corpus with metadata
newscorpus<- corpus(precorpus$Full.text,
                    docnames=precorpus$Document_ID,
                    docvar=data.frame(Year=precorpus$Publication.year,
                                      Subject= precorpus$Subject,
                                      Place=precorpus$Location,
                                      Title= precorpus$Title))
names(newscorpus) 
summary(newscorpus)  #summary of corpus


##########################
### ANALYSIS
##########################

#create document feature matrix from clean corpus + stem
help(tokens)
help(dfm)
dfm.tokens=tokens(newscorpus)
dfm.simple=dfm(dfm.tokens)
topfeatures(dfm.simple, n=50)

#advanced clean up dococument
dfm.tokens=tokens(newscorpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, 
                  remove_url = TRUE,
                  remove_separators = TRUE)
dfm.simple=dfm(dfm.tokens)
topfeatures(dfm.simple, n=50)

topfeatures(dfm.simple, n=10,   decreasing = FALSE)

#create document feature matrix from clean corpus + stem
help(tokens)
help(dfm)
dfm.tokens=tokens(newscorpus)
dfm.simple=dfm(dfm.tokens)
topfeatures(dfm.simple, n=50)

#advanced clean up dococument
dfm.tokens=tokens(newscorpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, 
                  remove_url = TRUE,
                  remove_separators = TRUE)
dfm.simple=dfm(dfm.tokens)
topfeatures(dfm.simple, n=50)
topfeatures(dfm.simple, n=10,   decreasing = TRUE)


# removing stop words
help(dfm_remove)

dfm<- dfm_remove(dfm.simple, stopwords("english"))
topfeatures(dfm, n=50)

# create a custom dictonary
swlist = c("s", "u","said", "the", "also", "say", "just", "like","for", 
           "us", "can", "may", "now", "year", "according", "mr", 
           "market", "share", "one", "company")


dfm.sw<- dfm_remove(dfm.simple, c(stopwords("english"),swlist))
topfeatures(dfm.sw, n=50)

#update for bigrams using tokens
toks.1<-tokens(newscorpus)   #creates tokens
toks.2<-tokens_remove(toks.1, stopwords("english"))  #remove stopwords from tokens
toks.3 <-tokens_ngrams(toks.2, n=2) # ngram =2
dfm.ngram2<- dfm(toks.3, verbose=TRUE)

topfeatures(dfm.ngram2, n=50)

#Stemming
dfm.stem=dfm_wordstem(dfm.sw)
topfeatures(dfm.stem, n=50)

#Sentiment Analysis
mydict <- dictionary(list(negative = c("detriment*", "bad*", "awful*", "terrib*", "horribl*"),
                          positive = c("good", "great", "super*", "excellent", "yay")))

dfm.sentiment=dfm_lookup(dfm.sw, mydict)
topfeatures(dfm.sentiment)

#########################
### WORD CLOUD ########
#########################
library(wordcloud)
set.seed(142)   #keeps cloud' shape fixed
dark2 <- brewer.pal(8, "Set1")   
freq<-topfeatures(dfm.stem, n=500)

wordcloud(names(freq), 
          freq, max.words=500, 
          scale=c(3, .1), 
          colors=brewer.pal(8, "Set1"))

#########################
### CORRELATIONS ########
#########################
# to evaluate sparcity
dfm
dfm.tm<-convert(dfm.sw, to="tm")
dfm.tm  
dfm.sparse<-removeSparseTerms(dfm.tm,0.5 )
dfm.sparse
#specifying a correlation limit of 0.5   
findAssocs(dfm.tm, 
           c("data", "tech", "big"), 
           corlimit=0.4)
findAssocs(dfm.tm, 
           c("public","model", "create" ), 
           corlimit=0.7)


#####################
# Hierc. Clustering 
#####################
dtmss <- removeSparseTerms(dfm.tm, 0.5)
d.dfm <- dist(t(dtmss), method="euclidian")
fit <- hclust(d=d.dfm, method="average")
hcd <- as.dendrogram(fit)

library(cluster)
k<-10
plot(hcd, ylab = "Distance", horiz = FALSE, 
     main = "Five Cluster Dendrogram", 
     edgePar = list(col = 2:3, lwd = 2:2))
rect.hclust(fit, k=k, border=1:5) # draw dendogram with red borders around the 5 clusters

library(ggplot2)

library(ggdendro)
ggdendrogram(fit, rotate = TRUE, size = 4, theme_dendro = FALSE,  color = "blue") +
  xlab("Features") + 
  ggtitle("Cluster Dendrogram")


##########################
### Topic Modeling
##########################


library(dplyr)
require(magrittr)
library(stringr)
library(NLP)

news_2015<-precorpus$Full.text


#Cleaning corpus
stop_words <- stopwords("SMART")
## additional junk words showing up in the data
stop_words <- c(stop_words, "said", "the", "also", "say", "just", "like","for", 
                "us", "can", "may", "now", "year", "according", "mr")
stop_words <- tolower(stop_words)


news_2015 <- gsub("'", "", news_2015) # remove apostrophes
news_2015 <- gsub("[[:punct:]]", " ", news_2015)  # replace punctuation with space
news_2015 <- gsub("[[:cntrl:]]", " ", news_2015)  # replace control characters with space
news_2015 <- gsub("^[[:space:]]+", "", news_2015) # remove whitespace at beginning of documents
news_2015 <- gsub("[[:space:]]+$", "", news_2015) # remove whitespace at end of documents
news_2015 <- gsub("[^a-zA-Z -]", " ", news_2015) # allows only letters
news_2015 <- tolower(news_2015)  # force to lowercase

## get rid of blank docs
news_2015 <- news_2015[news_2015 != ""]

# tokenize on space and output as a list:
doc.list <- strsplit(news_2015, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)


# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
term.table <- term.table[names(term.table) != ""]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

#############
# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (1)
W <- length(vocab)  # number of terms in the vocab (1741)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (56196)
term.frequency <- as.integer(term.table) 

# MCMC and model tuning parameters:
K <- 10
G <- 3000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
## display runtime
t2 - t1  

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

news_for_LDA <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

library(LDAvis)
library(servr)

# create the JSON object to feed the visualization:
json <- createJSON(phi = news_for_LDA$phi, 
                   theta = news_for_LDA$theta, 
                   doc.length = news_for_LDA$doc.length, 
                   vocab = news_for_LDA$vocab, 
                   term.frequency = news_for_LDA$term.frequency)

serVis(json, out.dir = 'class8pm', open.browser = TRUE)
