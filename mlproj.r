library(caret)
library(randomForest)

library(rpart)
library(rpart.plot)
set.seed(9999)
data <- read.csv("pml-training.csv" , na.strings = c("NA", ""))
testData3 <- read.csv("pml-testing.csv")

drops <- c(1:7)
data1 <- data[,-drops]
data2 <- data1[,colSums(is.na(data1))==0]


trainidx <- createDataPartition(y = data2$classe, p=0.7, list=FALSE)
trainingData <- data2[trainidx,]
testData <- data2[-trainidx,]

tree_tree <- rpart(classe ~ ., data=trainingData, method="class")
prp(tree_tree) # plotting using library(rpart.plot)


## fit the model
controlRf <- trainControl(method="cv", 5)
modelRf <- train(classe ~ ., data=trainingData, method="rf", trControl=controlRf, ntree=250)
modelRf

predictRf <- predict(modelRf, testData)
confusionMatrix(testData$classe, predictRf)


predictTest <- predict(modelRf, testData3)
predictTest
