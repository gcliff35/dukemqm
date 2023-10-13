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
library(readxl)
library(keras)

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
linear_pred
##Evaluate linear model##
residuals_linear <- residuals(linear)
MAE_linear <- mean(abs(residuals_linear))
MAE_linear
MSE_linear <- mean(residuals_linear^2)
MSE_linear
RMSE_linear <- sqrt(MSE_linear)
RMSE_linear
plot(linear, which = 1)


##Confusion matrix##
binary_linear <- as.factor(ifelse(linear_pred > 0.5, 1, 0))
head(binary_linear, n = 30)
binary_linear <- as.factor(binary_linear)
test_data$Default <- as.factor(test_data$Default)
set.seed(1234)
confusionMatrix(data = binary_linear, reference = test_data$Default)

##Logistic model##
logistic <- glm(Default ~ ., data = train_data, family = "binomial")
summary(logistic)
logistic_pred <- predict(object = logistic, newdata = test_data, type = "response")
binary_glm <- as.factor(ifelse(logistic_pred > 0.5, 1, 0))
head(binary_glm, n = 30)

###CONFUSIONMATRIX####
binary_glm <- as.factor(binary_glm)
test_data$Default <- as.factor(test_data$Default)
set.seed(1234)
confusionMatrix(data = binary_glm, reference = test_data$Default)

odds_function_income <- function(x){
  exp(-8.829e-06 * (x))
}

incomes <- c(15000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000, 130000, 140000, 150000, 160000)
incomes <- as.data.frame(incomes)
odds_income <- incomes %>%
  mutate(odds = (odds_function_income(incomes)))
odds_income
income_plot <- ggplot(odds_income, aes(x = incomes, y = odds)) + geom_line()
income_plot

##Random forest##
library(randomForest)
train_data$Default <- as.factor(train_data$Default)

randomForest <- randomForest(Default~., data = train_data)
randomForest_pred <- predict(randomForest, newdata = test_data)

binary_forest <- as.factor(ifelse(randomForest_pred > 0.5, 1, 0))
head(binary_forest, n = 30)
binary_forest <- as.factor(binary_forest)
test_data$Default <- as.factor(test_data$Default)
set.seed(1234)
confusionMatrix(data = binary_forest, reference = test_data$Default)

confusion <- confusionMatrix(randomForest_pred, test_data$Default)
print(confusion)


