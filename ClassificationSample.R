# Module 2 - Task 2 - CompleteResponses Classification
# Alican Tanaçan
# Version 2 - Random Forest (Manual Grid)

# Libraries ----
library(caret)
library(data.table)
library(tidyverse)
library(ggplot2)

# Loading the Data Frame ----
CompRespDF <- read.csv("CompleteResponses.csv")

# Preprocessing ----
CompRespDF$elevel <- as.ordered(CompRespDF$elevel)
CompRespDF$car <- as.factor(CompRespDF$car)
CompRespDF$zipcode <- as.factor(CompRespDF$zipcode)
CompRespDF$brand <- as.factor(CompRespDF$brand)

# Feature Selection ----
CompRespDFsub <- CompRespDF[c(1,2,6,7)]
CompRespDFsubsample <- CompRespDFsub[which(CompRespDFsub$salary < 80000 & CompRespDFsub$salary > 60000),]
ggplot(CompRespDFsubsample, aes(salary,age, color = brand))+geom_jitter(alpha = 1)

# Creating Data Partition ----
set.seed(66)
intrain <- createDataPartition(y = CompRespDFsubsample$brand,
                               times = 1, p = 0.75, list = FALSE)
trainingset <- CompRespDFsubsample[intrain,]
testingset <-CompRespDFsubsample[-intrain,]

# Random Forest Model----
set.seed(209)
rfcontrol2 <- trainControl(method = "repeatedcv",
                          number = 10, repeats = 1)
rfGrid1 <- expand.grid(mtry=c(1,2,3,4))
system.time(rfmodel2 <- train(brand~., data = trainingset, method = "rf",
                  trControl = rfcontrol2, tuneGrid = rfGrid1))
rfmodel2

rfpredict2 <- predict(rfmodel2, testingset)
summary(rfpredict2)

confusionMatrix(rfpredict2, testingset$brand)

postResample(rfpredict2, testingset$brand)

# Predictions on Incomplete Survey Data ----

IncompSurvData <- read.csv("SurveyIncomplete.csv")
IncompSurvDataSub <- IncompSurvData[c(1,2,6,7)]
IncompSurvDataSub$brand <- as.factor(IncompSurvDataSub$brand)
IncompSurvDataSubSample <- IncompSurvDataSub[which(IncompSurvDataSub$salary < 80000 & IncompSurvDataSub$salary > 60000),]

rfpredict3 <- predict(rfmodel2, IncompSurvDataSubSample)
summary(rfpredict3)

IncompSurvDataSubSample$brand <- rfpredict3
ggplot(IncompSurvDataSub, aes(salary,age, color = brand))+geom_jitter(alpha = 1)
