visits <- read.csv("park_visits.csv")
library(dplyr)
visits2016jul <- visits %>% filter(Month == 7 & Year == 2016)
summary(visits2016jul)
str(visits2016jul)
sort(table(visits2016jul$ParkType))
visits2016jul[which.max(visits2016jul$logVisits),1]
sort(tapply(visits2016jul$logVisits, visits2016jul$Region, mean))
cor(visits2016jul$logVisits,visits2016jul$cost)
ys <- filter(visits, ParkName == "Yellowstone NP")

ys_ts=ts(ys$logVisits,start=c(2010,1),freq=12)
plot(ys_ts)

colSums(is.na(visits)) 

visits = visits[rowSums(is.na(visits)) == 0, ]
visits$Month = as.factor(visits$Month)


training <-  filter(visits, visits$Year < 2015)
testing <- filter(visits, visits$Year > 2014)
mod <- lm(logVisits ~ laglogVisits, data=training)
summary(mod)
#out of sample R2 testing
summary(lm(logVisits ~ laglogVisits, data=testing))$r.squared 

mod1 <- lm(logVisits ~laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=training)
summary(mod1)
summary(lm(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=testing))$r.squared 


set.seed(201)

library(rpart)
library(rpart.plot)
visitsTree <- rpart(logVisits  ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=training, method="class", cp=0.0001)
prp(visitsTree)

PredictCART = predict(visitsTree, newdata = testing, type = "class")
table(testing$logVisits, PredictCART)
library(randomForest)
rm(PredictCART,visitsTree)


VisitForest = randomForest(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=training )$r.squared
summary(randomForest(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=training ))$r.squared
PredictForest = predict(VisitForest, newdata = testing)
table(testing$logVisits, PredictForest)
