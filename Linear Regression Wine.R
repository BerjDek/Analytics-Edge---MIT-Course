wine <- read.csv("wine.csv")
str(wine)
summary(wine)
model1 <- lm(Price ~ AGST, data= wine)
summary(model1)
model1$residuals
SSE <- sum(model1$residuals^2)
model2 <- lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
SSE <- sum(model2$residuals^2)
model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
SSE <- sum(model3$residuals^2)
model4 <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model4)
model5 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model5)
cor(wine$WinterRain, wine$Price)
model6 <- lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model6)
cor(wine)

winetest <- read.csv("wine_test.csv")
?predict
?lm
predicttest <- predict(model5, newdata = winetest)
predicttest
SSE <- sum((winetest$Price -predicttest)^2)
SST <- sum((winetest$Price - mean(wine$Price))^2)
1- SSE/SST
