#Andrew Riggs
#Data modification procedures for CS376B

#SETUP
#Install the needed packages
install.packages("arules")
install.packages("RWeka")

#Load the needed libraries
library(corrplot)
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

#Remove tag features based on correlation (FIRST PASS)
for(i in 6:ncol(gamedata)) {
  c <- cor(gamedata[[i]], gamedata$average_forever)
  if(c > -0.006158 && c < 0.020392) {
    gamedata[,i] <- NA
  }
}
gamedata <- gamedata[, colSums(is.na(gamedata)) != nrow(gamedata)]

#Remove tag features based on correlation (SECOND PASS)
for(i in 6:ncol(gamedata)) {
  c <- cor(gamedata[[i]], gamedata$average_forever)
  if(c > -0.012047 && c < 0.042394) {
    gamedata[,i] <- NA
  }
}
gamedata <- gamedata[, colSums(is.na(gamedata)) != nrow(gamedata)]

#Remove tag features based on correlation (THIRD PASS)
for(i in 6:ncol(gamedata)) {
  c <- cor(gamedata[[i]], gamedata$average_forever)
  if(c > -0.02281 && c < 0.05842) {
    gamedata[,i] <- NA
  }
}
gamedata <- gamedata[, colSums(is.na(gamedata)) != nrow(gamedata)]


#Remove tag features whose counts are lower than 648 (Second Median)
for(i in 6:ncol(gamedata)) {
  if(nrow(gamedata[gamedata[,i] == 1,]) < 648) {
    gamedata[,i] <- NA
  }
}
gamedata <- gamedata[, colSums(is.na(gamedata)) != nrow(gamedata)]

#Remove rows that no longer have any tags on them, chose Indie to compare because it is the most populated tag, and so should never be removed
for(i in 1:nrow(gamedata)) {
  if(rowSums(gamedata[i,6:ncol(gamedata)]) < 1) {
    gamedata[i,] <- NA
  }
}
gamedata <- gamedata[!is.na(gamedata$Indie),]

#Removing developer and publisher
gamedata$developer <- NULL
gamedata$publisher <- NULL

#Remove spaces in remaining feature names
names(gamedata)[7] <- "COOP"
names(gamedata)[8] <- "Early_Access"
names(gamedata)[9] <- "Open_World"
names(gamedata)[16] <- "Two_Dimensional"
names(gamedata)[17] <- "Pixel_Graphics"
names(gamedata)[18] <- "Point_And_Click"

#Remove instances where score is NA
gamedata <- gamedata[!is.na(gamedata$score_rank),]

#Create a version of the dataset with a target two class feature
gamedata.twoclass <- gamedata

gamedata.twoclass$likely_to_succeed <- "No"

for(i in 1:nrow(gamedata.twoclass)) {
  if(gamedata.twoclass[i,2] > 437) {
    gamedata.twoclass[i,20] <- "Yes"
  }
}

gamedata.twoclass$likely_to_succeed <- as.factor(gamedata.twoclass$likely_to_succeed)

gamedata.twoclass$average_forever <- NULL


#WRITING
#Write the data to an Rda file, a csv file, and an arff file
save(gamedata, file= 'modgamedata.Rda')
save(gamedata.twoclass, file= 'modgamedataTwo.Rda')
write.csv(gamedata, file= 'modGameData.csv')
write.csv(gamedata.twoclass, file= 'modGameDataTwo.csv')
write.arff(gamedata, file= 'modGameData.arff')
write.arff(gamedata.twoclass, file= 'modGameDataTwo.arff')

#Remove the data from the work environment
rm(gamedata)
rm(gamedata.twoclass)
rm(steamspy)
rm(i)
rm(c)
