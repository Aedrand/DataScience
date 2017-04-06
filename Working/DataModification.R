#Andrew Riggs
#Data modification procedures for CS376B

#LOADING
#Setup the data, loading from an Rda file.
#Storing in seperate frame because I'm not entirely sure how R works and don't want to overwrite the original data.
load('steamspy.Rda')
gamedata <- steamspy

#MODIFICATIONS
#Convert tag columns to either 1 or 0, based on whether or not the game can be categorized by that tag.
#Allows for an easier time categorizing by tags, and improves the overall readability of the data.
tagrows <- gamedata[,18:ncol(gamedata)]
tagrows[!is.na(tagrows)] <- 1
tagrows[is.na(tagrows)] <- 0
gamedata[,18:347] <- tagrows
rm(tagrows)

free <- gamedata[gamedata[,18] == 1,]
mean(free$average_forever)

mean(gamedata[gamedata[,18] == 1,]$average_forever)

#Remove features that are too highly correlated with each other, and are largely irrelevant
gamedata$appid <- NULL
gamedata$name <- NULL
gamedata[,4:9] <- NULL
gamedata[,5:8] <- NULL
gamedata$tags <- NULL

#Replace all commas in the developer and publisher columns to allow proper writing to csv.
gamedata$developer <- gsub(","," &",gamedata$developer)
gamedata$publisher <- gsub(","," &",gamedata$publisher)

#Remove rows where the price is NA, as these end up being unpublished or packaged software.
gamedata <- gamedata[!is.na(gamedata$price),]

#Convert price and score columns from chr to int.
gamedata$price <- as.numeric(gamedata$price)
gamedata$score_rank <- as.numeric(gamedata$score_rank)

#Remove tag features whose counts are lower than 20
for(i in 6:ncol(gamedata)) {
  if(nrow(gamedata[gamedata[,i] == 1,]) < 20) {
    gamedata[,i] <- NA
  }
}
gamedata <- gamedata[, colSums(is.na(gamedata)) != nrow(gamedata)]

for(i in 1:nrow(gamedata)) {
  if(rowSums(gamedata[i,6:ncol(gamedata)]) < 1) {
    gamedata[i,] <- NA
  }
}

NAS <- gamedata[is.na(gamedata$`Free to Play`),]
gamedata <- gamedata[]


#Create two other versions of the data, removing any entries below 100 average minutes and 1000 average minutes, respectively.
gamedata100 <- gamedata[gamedata$average_forever >= 100,]
gamedata1000 <- gamedata[gamedata$average_forever >= 1000,]

#FEATURE SELECTION



#WRITING
#Write the data to an Rda file and a csv file
save(gamedata, file= 'modgamedata.Rda')
save(gamedata100, file= 'modgamedata100.Rda')
save(gamedata1000, file= 'modgamedata1000.Rda')
gamedata <- as.matrix(gamedata)
write.csv(gamedata, file= 'modGameData.csv')
rm(gamedata)

