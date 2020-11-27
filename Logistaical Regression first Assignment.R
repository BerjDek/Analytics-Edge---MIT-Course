songs <- read.csv("songs.csv")
table(songs$year)
str(songs)
table(songs$artistname)
Michael <- subset(songs, artistname == "Michael Jackson" & Top10 == T )
table(Michael$Top10 , Michael$songtitle)
isTRUE(Michael$songtitle)
which(Michael$Top10 == T)
table(songs$timesignature)
which.max(songs$tempo)
songs[6206, "songtitle"]
SongsTrain <- subset(songs, year < 2010)
SongsTest <- subset(songs, year == 2010)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]

SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
Model1 <- glm(Top10 ~ . , data = SongsTrain, family=binomial)
summary(Model1)
rm(framingham,framinghamLog)
cor(SongsTrain$loudness, SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

predictTest = predict(SongsLog3, type="response", newdata=SongsTest)
table(SongsTest$Top10, predictTest > 0.45)
(309 + 19)/(309 + 5 + 40 + 19)
(309 + 5)/(309 + 5 + 40 + 19)

19/(19+40)
309/(309+5)
