##Load packages##
library(ggplot2)
library(tidyr)
library(tidyverse)
library(tidyselect)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)
library(tree)
library(randomForest)

##Naming file##
loan <- Loan_Default_Data_Science_Project

##Set factors##
loan$Default <- as.factor(loan$Default)
loan$Education <- as.factor(loan$Education)
loan$EmploymentType <- as.factor(loan$EmploymentType)
loan$MaritalStatus <- as.factor(loan$MaritalStatus)
loan$LoanPurpose <- as.factor(loan$LoanPurpose)
loan$HasCoSigner <- as.factor(loan$HasCoSigner)
loan$HasDependents <- as.factor(loan$HasDependents)
loan$HasMortgage <- as.factor(loan$HasMortgage)

##Splitting file##
set.seed(123456)
trainIndex <- createDataPartition(loan$Default, p = 0.70, list = FALSE)
train <- loan[trainIndex, ]
test <- loan[-trainIndex, ]
prop.table(table(train$Default))
prop.table(table(test$Default))

