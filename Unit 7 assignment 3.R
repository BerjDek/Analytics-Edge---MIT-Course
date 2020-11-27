tw <- read.csv("tweets1.csv", strings = F)

library(tm)
library(SnowballC)
library(RColorBrewer)

corpus = Corpus(VectorSource(tw$Tweet))
??tm
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
dtm = removeSparseTerms(dtm, 0.995)
dtm = DocumentTermMatrix(corpus)
dim(dtm)
?dim

install.packages("wordcloud")
library(wordcloud)
?wordcloud

colnames(dtm)
rownames(dtm)
m <- as.data.frame(as.table(dtm))
rm(m)


allTweets = as.data.frame(as.matrix(dtm))
colnames(allTweets)
colSums(allTweets)

wordcloud(colnames(allTweets), colSums(allTweets), , scale=c(2, 0.25))

corpus = Corpus(VectorSource(tw$Tweet))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, 
                c('apple',stopwords("english")) )
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))
?wordcloud


install.packages("RColorBrewer")
library(RColorBrewer)

display.brewer.all()
