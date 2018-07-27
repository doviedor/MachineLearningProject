

library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(randomForest)
library(RColorBrewer)
library(rattle)
library(lattice)
library(ggplot2)


# Clean the environment
rm(list = ls())

# Set Seed
set.seed(1980)

# Read the data from the Hard Disk
train_Crude <- read.csv("G://ML//pml-training.csv", na.strings = c("NA", "$DIV/0!",""))
test_Crude <- testing <- read.csv("G://ML//pml-testing.csv", na.strings = c("NA", "$DIV/0!",""))
train_Data <- train_Crude
test_Data <- test_Crude

# Verify the dimensions of the variables
dim(train_Data)
dim(test_Data)

#First, we remove columns that contain NA missing values.
train_Data <- train_Data[, colSums(is.na(train_Data)) == 0]
test_Data <- test_Data[, colSums(is.na(test_Data)) == 0]

# Second, we need to clean the data
classe <- train_Data$classe
trainRemove <- grepl("^X|timestamp|window", names(train_Data))
train_Data <- train_Data[, !trainRemove]
trainCleaned <- train_Data[, sapply(train_Data, is.numeric)]
trainCleaned$classe <- classe
testRemove <- grepl("^X|timestamp|window", names(test_Data))
test_Data <- test_Data[, !testRemove]
testCleaned <- test_Data[, sapply(test_Data, is.numeric)]
inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)
trainData <- trainCleaned[inTrain, ]
testData <- trainCleaned[-inTrain, ]

# Prediction with Decision Tree
treeModel <- rpart(classe ~ ., data=trainData, method="class")
prp(treeModel, box.palette = "auto", compress = TRUE, ycompress = TRUE)
rpart.plot(treeModel, type = 4, branch.lty = 6,box.palette = "auto", cex = .2)
#fancyRpartPlot(treeModel)
predict_DT <- predict(treeModel, testData, type = "class")
DT_CM <- confusionMatrix(testData$classe, predict_DT)
DT_CM
accuracy_DT <- postResample(predict_DT, testData$classe)
accuracy_DT
plot(DT_CM$table, col = DT_CM$byClass, main = paste("CM for Decision Tree"))



#Prediction with Cor
cor_plot <- cor(trainData[, -length(names(trainData))])
corrplot(cor_plot, method="color")
corrplot(cor_plot, type = "lower", tl.col = "black", tl.srt = 45)
corrplot(cor_plot, method = "circle")


# Prediction with RForest
controlRf <- trainControl(method="cv", 5)
model_Rforest <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=250)
model_Rforest
predict_Rforest <- predict(model_Rforest, testData)
confusionMatrix(testData$classe, predict_Rforest)
accuracy <- postResample(predict_Rforest, testData$classe)
accuracy

#oose <- 1 - as.numeric(confusionMatrix(testData$classe, predictRf)$overall[1])
#oose
result <- predict(model_Rforest, testCleaned[, -length(names(testCleaned))])
result



