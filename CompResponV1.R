# Module 2 - Task 2 - CompleteResponses Classification
# Alican Tanaçan
# Version 1 - Random Forest (Automatic Grid)

# Libraries ----
library(caret)
library(data.table)
library(tidyverse)

# Loading the Data Frame ----
CompRespDF <- read.csv("CompleteResponses.csv")

# Data Exploration ----
summary(CompRespDF)
str(CompRespDF)
hist(CompRespDF$salary)
hist(CompRespDF$age)
hist(CompRespDF$elevel)
hist(CompRespDF$car)
hist(CompRespDF$zipcode)
hist(CompRespDF$credit)
hist(CompRespDF$brand)

# Preprocessing ----
cor(CompRespDF, method = c("spearman"))
cor(CompRespDF, method = c("pearson"))
CompRespDF$elevel <- as.ordered(CompRespDF$elevel)
CompRespDF$car <- as.factor(CompRespDF$car)
CompRespDF$zipcode <- as.factor(CompRespDF$zipcode)
CompRespDF$brand <- as.factor(CompRespDF$brand)
str(CompRespDF)
plot(CompRespDF$zipcode,CompRespDF$salary)
plot(CompRespDF$car,CompRespDF$salary)
plot(CompRespDF$elevel,CompRespDF$salary)
plot(CompRespDF$brand,CompRespDF$salary)

# Feature Selection ----
CompRespDFsub <- CompRespDF[c(1,2,6,7)]
str(CompRespDFsub)

# Creating Data Partition ----
set.seed(66)
intrain <- createDataPartition(y = CompRespDFsub$brand,
                               times = 1, p = 0.75, list = FALSE)
trainingset <- CompRespDFsub[intrain,]
testingset <-CompRespDFsub[-intrain,]
nrow(trainingset)
nrow(testingset)

# Random Forest ----
set.seed(209)
rfcontrol1 <- trainControl(method = "repeatedcv",
                            number = 10, repeats = 1)
rfmodel1 <- train(brand~., data = trainingset, method = "rf",
                trControl = rfcontrol1, tuneLength = 2)
rfmodel1

varImp(rfmodel1)

confusionMatrix(rfmodel1)

rfpredict1 <- predict(rfmodel1, testingset)
rfpredict1

summary(rfpredict1)
