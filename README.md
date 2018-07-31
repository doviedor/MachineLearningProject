---
title: "Machine Learning Project"
author: "Dario Oviedo Rueda"
date: "July 7, 2018"
output: html_document
---

```{r setup, include=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r warning=FALSE}
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(randomForest)
library(RColorBrewer)
library(rattle)
```

<br/>

## 1. Introduction
<br/>

### a) Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).
<br/>

### b) Data
The training data for this project are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source:
http://groupware.les.inf.puc-rio.br/har. 

If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.
<br/>


## 2. Cleanning the environment

Even if it is not necesary, I am trying to implement this line in all my codes to have a clean environment

```{r rm}
rm(list = ls())
```

<br/>

## 3. Set the seed

Set the seed to get the same results

```{r set.seed}
set.seed(1980)
```

<br/>

## 4. Read the data

Read the data from the web. 

```{r Read data}
Train_data_web <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
Test_data_web <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

train_Crude <- read.csv(url(Train_data_web), na.strings = c("NA", "$DIV/0!",""))
test_Crude <- read.csv(url(Test_data_web), na.strings = c("NA", "$DIV/0!",""))
train_Data <- train_Crude
test_Data <- test_Crude
```

<br/>

## 5. Clean the data

We need to clean the data using different methods:

### a. Removing NA's
```{r NA}
train_Data <- train_Data[, colSums(is.na(train_Data)) == 0]
test_Data <- test_Data[, colSums(is.na(test_Data)) == 0]
```

### b. Removing garbash
```{r Garbash}
classe <- train_Data$classe
train_garb <- grepl("^X|timestamp|window", names(train_Data))
train_Data <- train_Data[, !train_garb]
train_Clean <- train_Data[, sapply(train_Data, is.numeric)]
train_Clean$classe <- classe
test_garb <- grepl("^X|timestamp|window", names(test_Data))
test_Data <- test_Data[, !test_garb]
testCleaned <- test_Data[, sapply(test_Data, is.numeric)]
```

<br/>

## 6. Making the partition
The next lines make the partition with 75% to training and using the cleaning data,make a new data framework

```{r Partition}
inTrain <- createDataPartition(train_Clean$classe, p=0.75, list=F)
trainData <- train_Clean[inTrain, ]
testData <- train_Clean[-inTrain, ]
```

<br/>

## 7. Correlation analysis

```{r Cor}
cor_plot <- cor(trainData[, -length(names(trainData))])
corrplot(cor_plot, type = "lower", tl.col = "black", tl.srt = 45)
```

<br/>

## 8. Prediction with Deicision tree
These are the decision tree.

```{r Decision}
treeModel <- rpart(classe ~ ., data=trainData, method="class")
prp(treeModel, box.palette = "auto", compress = TRUE, ycompress = TRUE)

predict_DT <- predict(treeModel, testData, type = "class")
DT_CM <- confusionMatrix(testData$classe, predict_DT)
DT_CM
accuracy_DT <- postResample(predict_DT, testData$classe)
accuracy_DT

```

<br/>

## 9. Prediction with Random forest

```{r Rforest}
controlRf <- trainControl(method="cv", 5)
model_Rforest <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=250)
model_Rforest


RFModel <- randomForest(classe ~ ., data=trainData)
predict_RF <- predict(RFModel, testData, type = "class")
RF_CM <- confusionMatrix(predict_RF, testData$classe)
RF_CM
accuracy_RF <- postResample(predict_RF, testData$classe)
accuracy_RF

```

<br/>

## 10. Results

The results are:   
Decision Tree <- `r accuracy_DT[1]`   
Random Forest <- `r accuracy_RF[1]`

<br/>

## 11. Applying the model to the Data

Random Forest has the better result with `r accuracy_RF[1]` and we use this model with the original data of Testing.

```{r Results}
result <- predict(model_Rforest, testCleaned[, -length(names(testCleaned))])
result

```
