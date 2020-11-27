loans <- read.csv("loans.csv")
str(loans)
summary(loans)
install.packages("mice")
library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed
install.packages("caTools")
library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.70)
train = subset(loans, split== TRUE)
test = subset(loans, split== FALSE)

model0 <-glm(not.fully.paid ~. , data = train, family = "binomial")
summary(model0)

logA=9.260+(-9.406e-03*700)
logB=9.260+(-9.406e-03*710)
ans1 <- logA - logB
ans1

loansPred <- predict(model0, newdata = test, type = "response")
test$predicted.risk <- loansPred
t <- table(test$not.fully.paid, loansPred > 0.5)
Ntest <- nrow(test)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4] 
Acc <- (TN+TP)/Ntest
Acc

library(ROCR)
ROCRpred = prediction(loansPred, test$not.fully.paid)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
#ROCRauc = performance(ROCRpred, "auc")
ROCRauc <- as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRauc