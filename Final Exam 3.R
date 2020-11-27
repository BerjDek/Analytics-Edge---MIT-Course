ratings <- read.csv("ratings.csv")
summary(ratings)
ratings = ratings[rowSums(is.na(ratings)) == 0, ]
ratings$userid = NULL

library(caret)
library(tidyr)
points <- ratings
preproc = preProcess(points)

pointsnorm = predict(preproc, points)
summary(points)
str(points)


distances = dist(pointsnorm, method = "euclidean")

dend = hclust(distances, method = "ward.D")

plot(dend, labels = FALSE)

set.seed(100)
KM <- kmeans(points, 4)
table(KM$cluster)
points$cluster <- KM$cluster
which.min(points$restaurants)
points$cluster[2805]

n <- as.data.frame(rbind(tapply(points$churches, points$cluster, mean)
,tapply(points$pools, points$cluster, mean)
,tapply(points$gyms, points$cluster, mean)
,tapply(points$bakeries, points$cluster, mean)
,tapply(points$cafes, points$cluster, mean)))


m <- as.data.frame(rbind(tapply(points$pools, points$cluster, mean)
,tapply(points$zoo, points$cluster, mean)
,tapply(points$beaches, points$cluster, mean)
,tapply(points$parks, points$cluster, mean)
,tapply(points$view_points, points$cluster, mean)
,tapply(points$gardens, points$cluster, mean)
,tapply(points$monuments, points$cluster, mean)))

mean(n)
