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
library(corrplot)
library(RColorBrewer)

##Load file##
data <- read_excel("Loan Default Data Science Project.xlsx")

##Drop Loan ID##
data_drop <- c("LoanID")
data_clean <- data[, !names(data) %in% data_drop]

##Set factors##
data_clean$HasMortgage <- ifelse(data_clean$HasMortgage == "Yes", 1, 0)
data_clean$HasDependents <- ifelse(data_clean$HasDependents == "Yes", 1, 0)
data_clean$HasCoSigner <- ifelse(data_clean$HasCoSigner == "Yes", 1, 0)

education_mapping <- c("High School"= 1, "Bachelor's" = 2, "Master's" = 3, "PhD" = 4)
data_clean$Education <- as.integer(factor(data_clean$Education, levels = names(education_mapping)))

EmploymentType_mapping <- c("Unemployed" = 0, "Self-employed" = 1,"Part-time" = 2, "Full-time" = 3)
data_clean$EmploymentType <- as.integer(factor(data_clean$EmploymentType, levels = names(EmploymentType_mapping)))

MaritalStatus_Mapping <- c("Single" = 0, "Divorced" = 1, "Married" = 2)
data_clean$MaritalStatus <- as.integer(factor(data_clean$MaritalStatus, levels = names(MaritalStatus_Mapping)))

LoanPurpose_mapping <- c("Other" = 0, "Auto" = 1, "Education" = 2, "Home" = 3, "Business" = 4)
data_clean$LoanPurpose <- as.integer(factor(data_clean$LoanPurpose, levels = names(LoanPurpose_mapping)))

data_clean

##OOS Split##
n <- count(data_clean)
(n-n%%3)/4/n
# 63836

test_data <- data_clean[1:63836,]
train_data <- data_clean[63837:255347,]

##Correlation##
Corr <- cor(data_clean)
Corr
CorrplotColor <- brewer.pal(n = 8, name = "BrBG")
corrplot(Corr, method = "color",col = CorrplotColor, tl.col = "black")

##Linear model##
linear <- lm(Default ~ ., data = train_data)
summary(linear)
linear_pred <- predict(linear, newdata = test_data)
