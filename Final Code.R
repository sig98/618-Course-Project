require(rJava)
require(RWeka)
require(partykit)
require(class)

trainData <- read.csv("trainset.csv")
testData <- read.csv("testset.csv")

summary(trainData)
summary(testData)

Formula <- Subscribed ~ .  
weights1 <- InfoGainAttributeEval(Formula, data = testData)
barplot(weights1, las=2)

#kNN Code

trainIndex <- which(trainData$Subscribed == 'no')
deleteIndex <- sample(trainIndex, length(trainIndex) - 22000)
trainData.subset <- trainData[-deleteIndex, ]
summary(trainData.subset)

Formula <- Subscribed ~ .
weights1 <- InfoGainAttributeEval(Formula, data = trainData.subset)
barplot(weights1, las=2)

newTrain <- trainData.subset
newTrain$month = factor(newTrain$month, 
                        levels = c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'), 
                        labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
newTrain$month <- as.numeric(newTrain$month)
newTrain$contact = factor(newTrain$contact, 
                          levels = c('cellular' , 'telephone'), 
                          labels = c(1,2))
newTrain$contact <- as.numeric(newTrain$contact)
newTrain$Subscribed <- as.factor(newTrain$Subscribed)
testData$Subscribed <- as.factor(testData$Subscribed)
summary(newTrain)

trainingData <- newTrain[c('age' , 'contact', 'nr.employed' , 'month')]
trainingData[-5] = scale(trainingData[-5]) 
trainData.target = newTrain[, 15]
summary(trainingData)

testingData <- testData[c('age' , 'contact', 'nr.employed' , 'month')]
testingData$month = factor(testingData$month, 
                           levels = c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'), 
                           labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
testingData$month <- as.numeric(testingData$month)
testingData$contact = factor(testingData$contact, 
                             levels = c('cellular' , 'telephone'), 
                             labels = c(1,2))
testingData$contact <- as.numeric(testingData$contact)
testingData[-5] = scale(testingData[-5]) 
testData.target = testData [, 15]
summary(testingData)

knnpred <- knn(train=trainingData, test=testingData, cl=trainData.target, k =109)
table (knnpred, testData.target)
