### BA - Text as Data  
### Author: JC Bonilla
### jb3379@nyu.edu

library(quanteda)
library(stm)
library(tm)
library(NLP)
library(openNLP)

#load .csv file with news articles
url<-
precorpus<- read.csv("~/Google Drive/GData/NewsText.csv", 
                     header=TRUE, stringsAsFactors=FALSE)
dim(precorpus) # dim of file; 107 files with 11 headers
names(precorpus)   # names of the headers
head(precorpus)
str(precorpus)


##########################
### Preprocessing
##########################
#create a corpus with metadata
require(quanteda)
help(corpus)
newscorpus<- corpus(precorpus$Full.text,
                    docnames=precorpus$Document_ID,
                    docvar=data.frame(Year=precorpus$Publication.year,Subject= precorpus$Subject))
#explore the corpus
names(newscorpus)   #to explore the output of the corpus function: "documents" "metadata"  "settings"  "tokens" 
summary(newscorpus)  #summary of corpus
head(newscorpus)

#clean corpus: removes punctuation, digits, converts to lower case
help(tokenize)
newscorpus<- toLower(newscorpus, keepAcronyms = FALSE) 
cleancorpus <- tokenize(newscorpus, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE,
                        verbose=TRUE)
#explore the clean corpus
head(cleancorpus)    # text into token form

#create document feature matrix from clean corpus + stem
help(dfm)
dfm.simple<- dfm(cleancorpus,
                 toLower = TRUE, 
                 ignoredFeatures =stopwords("english"), 
                 verbose=TRUE, 
                 stem=FALSE)
head(dfm.simple) #explore output of dfm


#to display most frequent terms in dfm
topfeatures<-topfeatures(dfm.simple, n=50)
topfeatures

#to create a custom dictionary  list of stop words
swlist = c("said", "mr", "can")
dfm.stem<- dfm(cleancorpus, toLower = TRUE, 
               ignoredFeatures = c(swlist, stopwords("english")),
               verbose=TRUE, 
               stem=TRUE)
topfeatures.stem<-topfeatures(dfm.stem, n=50)
topfeatures.stem

#exploration in context
kwic(cleancorpus, "programming", 2)
kwic(cleancorpus , "data analytics", window = 3)


#dfm with bigrams
help("tokenize")
cleancorpus <- tokenize(newscorpus, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE, 
                        ngrams=2, verbose=TRUE)

dfm.bigram<- dfm(cleancorpus, toLower = TRUE, 
                 ignoredFeatures = c(swlist, stopwords("english")),
                 verbose=TRUE, 
                 stem=FALSE)
topfeatures.bigram<-topfeatures(dfm.bigram, n=50)
topfeatures.bigram


#Sentiment Analysis
help(dfm)
mydict <- dictionary(list(negative = c("detriment*", "bad*", "awful*", "terrib*", "horribl*"),
                          postive = c("good", "great", "super*", "excellent", "yay")))
dfm.sentiment <- dfm(cleancorpus, dictionary = mydict)
topfeatures(dfm.sentiment)
View(dfm.sentiment)


#########################
### WORD CLOUD ########
#########################


library(wordcloud)
set.seed(142)   #keeps cloud' shape fixed
dark2 <- brewer.pal(8, "Set1")   
freq<-topfeatures(dfm.stem, n=500)

help("wordcloud")
wordcloud(names(freq), 
          freq, max.words=200, 
          scale=c(3, .1), 
          colors=brewer.pal(8, "Set1"))


#specifying a correlation limit of 0.5   
dfm.tm<-convert(dfm.stem, to="tm")
findAssocs(dfm.tm, 
           c("data", "tech", "big"), 
           corlimit=0.6)
findAssocs(dfm.tm, 
           c("public","model", "create" ), 
           corlimit=0.7)

##########################
### Topic Modeling
##########################

library(stm)

#Process the data for analysis.
help("textProcessor")
temp<-textProcessor(documents=precorpus$Full.text, metadata = precorpus)
names(temp)  # produces:  "documents", "vocab", "meta", "docs.removed" 
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta


#running stm for top 20 topics
help("stm")
prevfit <-stm(docs , vocab , 
              K=20, 
              verbose=TRUE,
              data=meta, 
              max.em.its=25)

topics <-labelTopics(prevfit , topics=c(1:20))
topics   #shows topics with highest probability words

#explore the topics in context.  Provides an example of the text 
help("findThoughts")
findThoughts(prevfit, texts = precorpus$Full.text,  topics = 10,  n = 2)

help("plot.STM")
plot.STM(prevfit, type="summary")
plot.STM(prevfit, type="labels", topics=c(15,4,5))
plot.STM(prevfit, type="perspectives", topics = c(19,10))

# to aid on assigment of labels & intepretation of topics
help(topicCorr)
mod.out.corr <- topicCorr(prevfit)  #Estimates a graph of topic correlations
plot.topicCorr(mod.out.corr)




