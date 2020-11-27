who<-read.csv("WHO.csv")
mean(who$Over60)
which.min(who$Over60)
who[183,]
which.max(who$LiteracyRate)  
who[44,]
?tapply
tapply(who$ChildMortality, who$Region, mean)
