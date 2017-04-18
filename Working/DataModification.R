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

#Remove tag features whose counts are lower than 1051/648
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

#######TEMP#############

tempList <- data.frame()

s <- 0
for(i in 6:ncol(gamedata)) {
  s <- 0
  #writeLines(paste("Thing for", colnames(gamedata)[i], sep = " "))
  s <- cor(gamedata[[i]], gamedata$average_forever)
  s <- s #/nrow(gamedata)
  tempList[i - 5,1] <- s
  #print(s)
}

summary(tempList)

r <- sum(tempList$V1)
r <- r / nrow(tempList)
print(r)

corData <- cor(gamedata[,4:ncol(gamedata)])
png("corrMod.png", width = 10000, height = 10000)
corrplot(corData, main = "Correlation Plot")
dev.off()

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


tempList2 <- data.frame()

s <- 0
for(i in 6:ncol(gamedata)) {
  s <- 0
  #writeLines(paste("Thing for", colnames(gamedata)[i], sep = " "))
  s <- sum(gamedata[,i])
  s <- s #/nrow(gamedata)
  tempList2[i - 5,1] <- s
  #print(s)
}

summary(tempList2)

########################

#Create two other versions of the data, removing any entries below 100 average minutes and 1000 average minutes, respectively.
#gamedata100 <- gamedata[gamedata$average_forever >= 100,]
#gamedata1000 <- gamedata[gamedata$average_forever >= 1000,]

#Removing developer and publisher for now
gamedata$developer <- NULL
gamedata$publisher <- NULL

gamedata$likely_to_succeed <- "No"

for(i in 1:nrow(gamedata)) {
  if(gamedata[i,2] > 437) {
    gamedata[i,20] <- "Yes"
  }
}

gamedata$likely_to_succeed <- as.factor(gamedata$likely_to_succeed)

gamedata$average_forever <- NULL

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
