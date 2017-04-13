#Andrew Riggs
#Data modification procedures for CS376B

#SETUP
#Install the needed packages
install.packages("arules")
install.packages("RWeka")

#Load the needed libraries
library(arules)
library(RWeka)

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

#Remove rows that no longer have any tags on them
for(i in 1:nrow(gamedata)) {
  if(rowSums(gamedata[i,6:ncol(gamedata)]) < 1) {
    gamedata[i,] <- NA
  }
}
gamedata <- gamedata[!is.na(gamedata$`Free to Play`),]

#Create two other versions of the data, removing any entries below 100 average minutes and 1000 average minutes, respectively.
#gamedata100 <- gamedata[gamedata$average_forever >= 100,]
#gamedata1000 <- gamedata[gamedata$average_forever >= 1000,]

gamedata$developer <- NULL
gamedata$publisher <- NULL
gamedata$score_rank[is.na(gamedata$score_rank)] <- 0

#Create a version of the data with a binned target feature
gamedata.binned <- gamedata
gamedata.binned$average_forever <- discretize(gamedata$average_forever, "frequency", categories = 10)



#WRITING
#Write the data to an Rda file, a csv file, and an arff file
save(gamedata, file= 'modgamedata.Rda')
save(gamedata.binned, file= 'modgamedataBinned.Rda')
#save(gamedata100, file= 'modgamedata100.Rda')
#save(gamedata1000, file= 'modgamedata1000.Rda')
write.csv(gamedata, file= 'modGameData.csv')
write.csv(gamedata.binned, file= 'modGameDataBinned.csv')
write.arff(gamedata, file= 'modGameData.arff')
write.arff(gamedata.binned, file= 'modGameDataBinned.arff')

#Remove the data from the work environment
rm(gamedata)
rm(gamedata.binned)
#rm(gamedata100)
#rm(gamedata1000)
rm(steamspy)
rm(i)
