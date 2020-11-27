bank <- read.csv("bank.csv")
summary(bank)
sort(tapply(bank$duration, bank$job, mean))
library(ggplot2)

ggplot(data=bank, aes(x=job, y= duration)) + geom_boxplot()

cor(bank$emp.var.rate, bank$cons.price.idx, bank$cons.conf.idx)
cor(bank[,16:20])


set.seed(201)

library(caTools)

spl = sample.split(bank$y, 0.7)
Training = subset(bank, spl == TRUE)
Testing = subset(bank, spl == FALSE)

mod1 <- glm(y ~  age + job + marital + education + default + housing + loan+ contact+ month+ day_of_week+ campaign + pdays+ previous + poutcome + emp.var.rate+ cons.price.idx + cons.conf.idx, data=Training, family=binomial)
summary(mod1)

predictTrain = predict(mod1, type="response")
summary(predictTrain)
table(Training$y, predictTrain > 0.5)
58+328


TestPrediction = predict(mod1, newdata=Testing, type="response")
table(Testing$y, TestPrediction > 0.5)
50+133

library(ROCR)
ROCRpred = prediction(TestPrediction, Testing$y)
as.numeric(performance(ROCRpred, "auc")@y.values)

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


set.seed(201)
install.packages("caret")
library(lattice)
library(ggplot2)
library(caret)
install.packages("e1071")
library(e1071)

numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = (1:50)*0.001) 
mod2 <- train(y ~  age + job + marital + education + default + housing + loan+ contact+ month+ day_of_week+ campaign + pdays+ previous + poutcome + emp.var.rate+ cons.price.idx + cons.conf.idx, data=Training,method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
plot(mod2)
mod2$bestTune

library(rpart)
library(rpart.plot)
Tree = rpart(y ~  age + job + marital + education + default + housing + loan+ contact+ month+ day_of_week+ campaign + pdays+ previous + poutcome + emp.var.rate+ cons.price.idx + cons.conf.idx, data=Training, method="class")
prp(Tree)
PredictCART = predict(Tree, newdata = Testing, type = "class")
table(Testing$y, PredictCART)
(1303+28)/(1303+28+20+149)
