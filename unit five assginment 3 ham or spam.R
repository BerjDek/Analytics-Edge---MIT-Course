emails = read.csv("emails.csv", stringsAsFactors=FALSE)
sum(emails$spam)

(max(nchar(emails$text)))
(min(nchar(emails$text)))
which.min((nchar(emails$text)))
library(tm)

corpus = VCorpus(VectorSource(emails$text))

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, removePunctuation)


corpus = tm_map(corpus, removeWords, stopwords("english"))

corpus = tm_map(corpus, stemDocument)


dtm = DocumentTermMatrix(corpus)

dtm

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse <- as.data.frame(as.matrix(spdtm))

colnames(emailsSparse) = make.names(colnames(emailsSparse))


which.max(colSums(emailsSparse))


emailsSparse$spam <- emails$spam
emailsSparse1 <- subset(emailsSparse, spam == 0)
sort(colSums(emailsSparse1))

emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

spamLog = glm(spam~., data=train, family="binomial")


library(rpart)
library(rpart.plot)
spamCART = rpart(spam~., data=train, method="class")
prp(spamCART)

library(randomForest)
set.seed(123)
spamRF = randomForest(spam~., data=train)


predTrainLog = predict(spamLog, data= train, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]
table(predTrainLog < 0.00001)
table(predTrainLog >= 0.00001 & predTrainLog <=0.99999)
summary(spamLog)

table(train$spam, predTrainLog > 0.5)
(3052+954)/(3052+954+4)

library(ROCR)
LogROCRpred = prediction(predTrainLog, train$spam)
as.numeric(performance(LogROCRpred, "auc")@y.values)


trainCARTpred = predict(spamCART, data=train)
trainCARTpred.prob = trainCARTpred[,2]
table(train$spam, trainCARTpred.prob >= 0.5)
(2885+894)/nrow(train)


trainpredROCR = prediction(trainCARTpred.prob, train$spam)
performance(trainpredROCR, "auc")@y.values


table(train$spam, predTrainRF>0.5)
(3013+914)/(3013+39+44+914)

trainRFROCR = prediction(predTrainRF, train$spam)
performance(trainRFROCR, "auc")@y.values

testLogPred = predict(spamLog, newdata=test, type="response")
table(test$spam, testLogPred > 0.5)
(1257+376)/nrow(test)


LogROCRpred_test = prediction(testLogPred, test$spam)
as.numeric(performance(LogROCRpred_test, "auc")@y.values)

testCARTpred = predict(spamCART, newdata=test)[,2]
table(test$spam, testCARTpred >= 0.5)
(1228+386)/nrow(test)

testpredROCR = prediction(testCARTpred, test$spam)
performance(testpredROCR, "auc")@y.values


testRFPred = predict(spamRF, newdata=test, type="prob")[,2]
table(test$spam, testRFPred>0.5)
(1290+385)/nrow(test)

testRFROCR = prediction(testRFPred, test$spam)
performance(testRFROCR, "auc")@y.values