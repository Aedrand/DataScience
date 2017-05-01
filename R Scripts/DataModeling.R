#Andrew Riggs
#Data modeling procedures for CS376B

#####SETUP#####
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
install.packages("neuralnet")

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
library(neuralnet)

#Load the datasets
load('modgamedata.Rda')
load('modgamedataTwo.Rda')
load('modgamedataBinary.Rda')

######TRAIN AND TEST SEPERATION#####
#Seperate the data into training and test sets
set.seed(1)
trainSet <- createDataPartition(gamedata$average_forever, p=.6)[[1]]
gamedata.train <- gamedata[trainSet,]
gamedata.test <- gamedata[-trainSet,]
rm(trainSet)

#Seperate the two class data into training and test sets
trainSet <- createDataPartition(gamedata.twoclass$likely_to_succeed, p=.6)[[1]]
gamedata.two.train <- gamedata.twoclass[trainSet,]
gamedata.two.test <- gamedata.twoclass[-trainSet,]
rm(trainSet)

#Seperate the binary data into training and test sets
trainSet <- createDataPartition(gamedata.binary.twoclass$likely_to_succeed, p=.6)[[1]]
gamedata.binary.train <- gamedata.binary.twoclass[trainSet,]
gamedata.binary.test <- gamedata.binary.twoclass[-trainSet,]
rm(trainSet)

#####MODELING#####

##C4.5##
C45.gamedata <- J48(likely_to_succeed ~ ., data=gamedata.two.train)

summary(C45.gamedata)

#Output a plot of the tree
png("gameC4.5Tree.png", width = 1500, height = 1500)
plot(C45.gamedata)
dev.off()

###RIPPER###
RIPPER.gamedata <- JRip(likely_to_succeed ~ ., data=gamedata.two.train)

print(RIPPER.gamedata)

###Naive Bayes###
Bayes.gamedata <- naiveBayes(likely_to_succeed ~ .,data = gamedata.two.train)
summary(Bayes.gamedata)

###Nueral Network###
#Learn the dataset
# Workaround
gamedata.nn.formula <- as.formula(paste("likely_to_succeed ~ ", 
                                         paste(names(gamedata.binary.train[!names(gamedata.binary.train) %in% 'likely_to_succeed']), 
                                               collapse = " + "), sep=""))

# Build NN model with default hidden layer (1 hidden layer with 1 node)
gamedata.nn1 <- neuralnet(gamedata.nn.formula, data = gamedata.binary.train, hidden = 5)

# Plot the network
png("plsPlot.png", width = 1000, height = 1000)
plot(gamedata.nn1)
dev.off()

nn.predict <- compute(gamedata.nn1, gamedata.binary.test[,1:18])
nn.eval <- confusionMatrix(nn.predict$net.result, gamedata.binary.test$likely_to_succeed)
print(nn.eval)

###SVM###
gamedata.two.svm <- svm(likely_to_succeed ~., data = gamedata.two.train)

# Plot the discrete model
plot(gamedata.two.svm, gamedata.two.train)

#####EVALUATION#####

###C4.5### 
C45.predict <- predict(C45.gamedata, gamedata.two.test)
C45.eval <- confusionMatrix(C45.predict, gamedata.two.test$likely_to_succeed)
print(C45.eval)

###RIPPER###
RIPPER.predict <- predict(RIPPER.gamedata, gamedata.two.test)
RIPPER.eval <- confusionMatrix(RIPPER.predict, gamedata.two.test$likely_to_succeed)
print(RIPPER.eval)

###Naive Bayes###
Bayes.predict <- predict(Bayes.gamedata, gamedata.two.test)
Bayes.eval <- confusionMatrix(Bayes.predict, gamedata.two.test$likely_to_succeed)
print(Bayes.eval)

###SVM###
predict.svm <- predict(gamedata.two.svm, gamedata.two.test)
svm.eval <- confusionMatrix(predict.svm, gamedata.two.test$likely_to_succeed)
print(svm.eval)