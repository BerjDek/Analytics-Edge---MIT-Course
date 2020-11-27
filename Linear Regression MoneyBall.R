baseball <- read.csv("baseball.csv")
str(baseball)
moneyball <- subset(baseball, Year < 2002)
moneyball$RD <- moneyball$RS - moneyball$RA
plot(moneyball$RD , moneyball$W)
WinsReg <- lm(W ~ RD, moneyball)
summary(WinsReg)
cor(moneyball)
RunsReg <- lm(RS~ OBP + SLG, data = moneyball )
b <- (2737.77 * 		0.361)  + (1584.91 * 	0.500) - 804.63 

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)
