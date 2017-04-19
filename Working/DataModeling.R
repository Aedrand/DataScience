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


#######TESTING ZONE###############
load('modgamedata.Rda')
load('modgamedataTwo.Rda')

#TRAIN AND TEST SEPERATION
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

#C4.5
C45.gamedata <- J48(likely_to_succeed ~ ., data=gamedata.two.train)

summary(C45.gamedata)

#Output a plot of the tree
png("gameC4.5Tree.png", width = 1000, height = 1000)
plot(C45.gamedata)
dev.off()

#RIPPER
RIPPER.gamedata <- JRip(likely_to_succeed ~ ., data=gamedata.two.train)

print(RIPPER.gamedata)

#Naive Bayes
Bayes.gamedata <- naiveBayes(likely_to_succeed ~ .,data = gamedata.two.train)
summary(Bayes.gamedata)

#Nueral Network
# Create AND and XOR data frames
and.train <- data.frame(d1=c(0, 0, 1, 1), 
                        d2=c(0, 1, 0, 1), 
                        t=c(0, 0, 0, 1))[rep(seq(1, 4),10),]
xor.train <- data.frame(d1=c(0, 0, 1, 1),
                        d2=c(0, 1, 0, 1), 
                        t=c(0, 1, 1, 0))[rep(seq(1, 4),10),]

# Plot AND
plot(and.train[1:4,1:2], pch=c(1, 19)[and.train$t + 1], main="AND")

# Plot XOR
plot(xor.train[1:4,1:2], pch=c(1, 19)[xor.train$t + 1], main="XOR")

# Learn AND with 0 hidden layers
and.model.nn <- neuralnet(t ~ d1 + d2, data=and.train, hidden=0)
and.pred.nn <- compute(and.model.nn, and.train[,1:2])
and.eval.conMat <- confusionMatrix(round(and.pred.nn$net.result), and.train$t)
print(and.eval.conMat$table)

# Plot AND result
plot(and.model.nn)

# Learn XOR with 0 hidden layers
xor.model.nn <- neuralnet(t ~ d1 + d2, data=xor.train, hidden=0)
xor.pred.nn <- compute(xor.model.nn, xor.train[,1:2])
xor.eval.conMat <- confusionMatrix(round(xor.pred.nn$net.result), xor.train$t)
print(xor.eval.conMat$table)

# Plot XOR result
plot(xor.model.nn)

# Learn XOR with 1 hidden layer (with two nodes)
xor.model.nn <- neuralnet(t ~ d1 + d2, data=xor.train, 
                          hidden=2, threshold=0.0001)
xor.pred.nn <- compute(xor.model.nn, xor.train[,1:2])
xor.eval.conMat <- confusionMatrix(round(xor.pred.nn$net.result), xor.train$t)
print(xor.eval.conMat$table)

# Plot XOR result
plot(xor.model.nn)

#Learn the dataset
# Workaround for "species ~ ."
gamedata.nn.formula <- as.formula(paste("average_forever ~ ", 
                                         paste(names(gamedata.train[!names(gamedata.train) %in% 'average_forever']), 
                                               collapse = " + "), sep=""))

# Build NN model with default hidden layer (1 hidden layer with 1 node)
gamedata.nn1 <- neuralnet(gamedata.nn.formula, data = gamedata.train)

# Plot the network
plot(gamedata.nn1)

# Build NN model with 2 hidden layers (3 and 4 nodes)
# Use backpropagation with 0.01 learning rate
gamedata.nn2 <- neuralnet(gamedata.nn.formula, 
                                 data=gamedata.train, 
                                 hidden=c(3,4), 
                                 algorithm="backprop", 
                                 learningrate=0.02)

# Plot the network
plot(gamedata.nn2)


#SVM
gamedata.two.svm <- svm(likely_to_succeed ~., data = gamedata.two.train)
gamedata.svm <- svm(average_forever ~ ., data = gamedata.train)

# Plot the discrete model
plot(gamedata.svm, gamedata.train)
plot(gamedata.two.svm, gamedata.two.train)

#EVALUATION
#C4.5 
C45.predict <- predict(C45.gamedata, gamedata.two.test)
C45.eval <- confusionMatrix(C45.predict, gamedata.two.test$likely_to_succeed)
print(C45.eval)

#RIPPER
RIPPER.predict <- predict(RIPPER.gamedata, gamedata.two.test)
RIPPER.eval <- confusionMatrix(RIPPER.predict, gamedata.two.test$likely_to_succeed)
print(RIPPER.eval)

#Naive Bayes
Bayes.predict <- predict(Bayes.gamedata, gamedata.two.test)
eval_model(Bayes.predict, gamedata.two.test)
