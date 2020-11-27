
wiki <- read.csv("wiki.csv", stringsAsFactors = F)
wiki$Vandal <- as.factor(wiki$Vandal)
sum(wiki$Vandal == 1)
library(tm)
library(SnowballC)
corpusAdded <- Corpus(VectorSource(wiki$Added))

corpusAdded  <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded  <- tm_map(corpusAdded, stemDocument)

dtmAdded <- DocumentTermMatrix(corpusAdded)
length(stopwords("english"))
dtmAdded

frequencies = DocumentTermMatrix(corpusAdded)
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved  <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved  <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal
set.seed(123)
library(caTools)
sample.split(wikiWords, 70)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, split==TRUE)

wikiTest = subset(wikiWords, split==FALSE)
table(wikiTest$Vandal)
wikiCART = rpart(Vandal ~ ., data=wikiTrain, method="class")
testPredictCART = predict(wikiCART, newdata=wikiTest, type="class")
table(wikiTest$Vandal, testPredictCART)
prp(wikiCART)
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP)
wikiTrain2 = subset(wikiWords2, split==TRUE)

wikiTest2 = subset(wikiWords2, split==FALSE)
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
testPredictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
prp(wikiCART2)
table(wikiTest2$Vandal, testPredictCART2)
(602+61)/(602+16+61+484)
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded )

wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
 wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART3)