# Module 2 - Task 2: Multiple Regression in R
# Alican Tanaçan
# Version 1 - Task Exploration Before Preprocessing

#3 Libraries ----
library(caret)
library(ggplot2)
library(corrplot)

## Import Data ----
ExistingProductsData <- read.csv("existingproductattributes2017.csv")

## Preprocessing ----
# To remove NA column:
ExistingProductsData$BestSellersRank <- NULL
# To give binary values to every product type:
ExProdDummy <- dummyVars(" ~ .", data = ExistingProductsData)
ExProdReady <- data.frame(predict(ExProdDummy, newdata = ExistingProductsData))
# To see the correlation between all variables:
corrData <- cor(ExProdReady)
corrplot(corrData, is.corr = FALSE)
corrData
# To look certain correlations for PC, Laptop, Netbook and Smartphone:
ExProdReady[,c(1:4, 8:9, 11:13)] <- list(NULL)
corrData <- cor(ExProdReady)
corrplot(corrData)
