#Andrew Riggs
#Data modeling procedures for CS376B

#SETUP
#Install packages
install.packages("car")
install.packages("lattice")
install.packages("Hmisc")
install.packages("caret")
install.packages("RWeka")
install.packages("rpart")
install.packages("partykit")
install.packages("arules")
install.packages("e1071")
install.packages("OneR")
install.packages("mlbench")

#Load libraries
library(car)
library(lattice)
library(Hmisc)
library(caret)
library(RWeka)
library(rpart)
library(partykit)
library(arules)
library(e1071)
library(OneR)
library(mlbench)

#LOADING
#Load the datasets
load('modgamedata.Rda')
load('modgamedataBinned.Rda')

#TRAIN AND TEST SEPERATION
#Seperate the data into training and test sets
set.seed(1)
trainSet <- createDataPartition(gamedata$average_forever, p=.6)[[1]]
gamedata.train <- gamedata[trainSet,]
gamedata.test <- gamedata[-trainSet,]
rm(trainSet)

#Seperate the binned data into training and test sets
trainSet <- createDataPartition(gamedata.binned$average_forever, p=.6)[[1]]
gamedata.binned.train <- gamedata.binned[trainSet,]
gamedata.binned.test <- gamedata.binned[-trainSet,]
rm(trainSet)

#MODELING
#C4.5
C45.gamedata <- J48(average_forever ~ ., data=gamedata.binned.train)

summary(C45.gamedata)

#png("gameC4.5Tree.png", width = 1000, height = 1000)
plot(C45.gamedata)
#dev.off()

#RIPPER
RIPPER.gamedata <- JRip(average_forever ~ ., data=gamedata.binned.train)

print(RIPPER.gamedata)

#Naive Bayes
Bayes.gamedata <- naiveBayes(average_forever ~ .,data = gamedata.binned.train)
summary(Bayes.gamedata)

#EVALUATION
#C4.5 
C45.predict <- predict(C45.gamedata, gamedata.binned.test)
C45.eval <- confusionMatrix(C45.predict, gamedata.binned.test$average_forever)
print(C45.eval)

#RIPPER
RIPPER.predict <- predict(RIPPER.gamedata, gamedata.binned.test)
RIPPER.eval <- confusionMatrix(RIPPER.predict, gamedata.binned.test$average_forever)
print(RIPPER.eval)

#Naive Bayes
Bayes.predict <- predict(Bayes.gamedata, gamedata.binned.test)
eval_model(Bayes.predict, gamedata.binned.test)
