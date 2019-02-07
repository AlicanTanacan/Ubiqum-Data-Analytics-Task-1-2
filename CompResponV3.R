# Module 2 - Task 2 - CompleteResponses Classification
# Alican Tanaçan
# Version 3 - C5.0 Algorithmm

# Libraries ----
library(caret)
library(data.table)
library(tidyverse)

# Loading the Data Frame ----
CompRespDF <- read.csv("CompleteResponses.csv")

# Preprocessing ----
CompRespDF$elevel <- as.ordered(CompRespDF$elevel)
CompRespDF$car <- as.factor(CompRespDF$car)
CompRespDF$zipcode <- as.factor(CompRespDF$zipcode)
CompRespDF$brand <- as.factor(CompRespDF$brand)

# Feature Selection ----
CompRespDFsub <- CompRespDF[c(1,2,6,7)]

# Creating Data Partition ----
set.seed(147)
intrain <- createDataPartition(y = CompRespDFsub$brand,
                               times = 1, p = 0.75,
                               list = FALSE)
trainingset <- CompRespDFsub[intrain,]
testingset <-CompRespDFsub[-intrain,]

# C5.0 ----
set.seed(174)
levels(trainingset$brand) <- c("acer","sony")
CrossValidControl <- trainControl(method = "repeatedcv",
                       repeats = 3,
                       summaryFunction = defaultSummary,
                       classProbs = TRUE)
CGrid2 <- expand.grid(.model="tree",.trials = c(1:20),.winnow = FALSE)
C5model1 <- train(brand~.,
                data = trainingset,
                method = "C5.0",
                metric = "Accuracy",
                tuneGrid = CGrid2,
                trControl = CrossValidControl)
C5model1

varImp(C5model1)

c5predict2 <- predict(C5model1, testingset)
summary(c5predict2)
