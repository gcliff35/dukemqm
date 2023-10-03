loan <- Loan_Default_Data_Science_Project

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

###START###
#####SPLITTING######
set.seed(123456)
trainIndex <- createDataPartition(loan$Default, p = 0.70, list = FALSE)
train <- loan[trainIndex, ]
test <- loan[-trainIndex, ]
prop.table(table(train$Default))
prop.table(table(test$Default))
#########

###LINEAR###
lm1 <- lm(Default ~ . , data = loan)
summary(lm1)

####GLM###. https://rpubs.com/kevinTongam/Logisticregression
train$Default <- as.factor(train$Default)
log.reg <- glm(Default ~ ., data = train, family = "binomial")
summary(log.reg)

#####TUNEDGLM####
log.reg2 <- glm(Default ~ Age + Income + LoanAmount + CreditScore + MonthsEmployed + NumCreditLines + InterestRate + DTIRatio + Education + EmploymentType + MaritalStatus + HasMortgage + HasDependents + LoanPurpose + HasCoSigner, data = train, family = "binomial")
summary(log.reg2)

####ODDSFORINCOME####
odds_function_income <- function(x){
  exp(-8.855e-06 * (x))
}

incomes <- c(15000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000, 130000, 140000, 150000, 160000)
incomes <- as.data.frame(incomes)
odds_income <- incomes %>%
  mutate(odds = (odds_function_income(incomes)))
odds_income
income_plot <- ggplot(odds_income, aes(x = incomes, y = odds)) + geom_line()
income_plot

####ODDSFORLOANAMOUNT####
odds_function_loan <- function(y){
  exp(-4.267e-06 * (y))
}

loans <- c(5000, 15000, 30000, 45000, 60000, 75000, 90000, 105000, 120000, 135000, 150000, 165000, 180000, 195000, 210000, 225000, 240000, 255000)
loans <- as.data.frame(loans)
odds_loan <- loans %>%
  mutate(odds2 = (odds_function_loan(loans)))
odds_loan
loan_plot <- ggplot(odds_loan, aes(x = loans, y = odds2)) + geom_line()
loan_plot

####ODDSFORCREDITSCORE#####
odds_function_cs <- function(z){
  exp(-7.893e-04 * (z))
}

scores <- c(300, 330, 360, 390, 420, 450, 480, 510, 540, 570, 600, 630, 660, 690, 720, 750, 780, 810, 840, 870)
scores <- as.data.frame(scores)
odds_scores <- scores %>%
  mutate(odds3 = (odds_function_cs(scores)))
odds_scores
scores_plot <- ggplot(odds_scores, aes(x = scores, y = odds3)) + geom_line()
scores_plot

###PREDICTION####
predictions_glm <- predict(object = log.reg2, newdata = test[-17], type = "response")
head(predictions_glm, n = 30)

binary_glm <- as.factor(ifelse(predictions_glm > 0.5, 1, 0))
head(binary_glm, n = 30)

###CONFUSIONMATRIX####
binary_glm <- as.factor(binary_glm)
test$Default <- as.factor(test$Default)
set.seed(1234)
confusionMatrix(data = binary_glm, reference = test$Default)

####AUC######
pred_ROCR <- prediction(predictions_glm, test$Default_glm)
auc_ROCR <- performance(pred_ROCR, measure = 'auc')
plot(performance(pred_ROCR, measure = 'tpr', x.measure = 'fpr'), colorize = TRUE,
     print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
####AREAUNDERCURVE####
paste(signif(auc_ROCR@y.values[[1]]))

library(pROC)
true_labels <- test$Default
roc_obj <- roc(true_labels, predictions_glm)
plot(roc_obj, main="ROC Curve", col="blue", lwd=2)
abline(h = 0, v = 1, col = "gray", lty = 2) 
auc(roc_obj)
text(0.7, 0.2, paste("AUC:", round(auc(roc_obj), 3)), adj=c(0.5,0.5))
roc_obj <- roc(true_labels, predictions_glm) 
coords(roc_obj, "best", best.method="youden")
###STOP###




#######DECISIONTREE##########
#decision <- rpart(Default ~., data = train, method = 'class')
#rpart.plot(decision, type = 2, extra = 106)
#rpart.plot(decision,
           #type = 2, extra = 106,
           #under = FALSE, fallen.leaves = TRUE,
          #digits = 2, varlen = 0, faclen = 0, roundint = TRUE,
           #cex = NULL, tweak = 1,
           #clip.facs = FALSE, clip.right.labs = TRUE,
           #snip = FALSE,
           #box.palette = "auto", shadow.col = 0)
#summary(decision)
##THIS IS A BAD MODEL FOR THIS USE CASE###

decision <- tree(Default ~ Age + Income + LoanAmount + CreditScore + MonthsEmployed + NumCreditLines + InterestRate + DTIRatio + Education + EmploymentType + MaritalStatus + HasMortgage + HasDependents + LoanPurpose + HasCoSigner, data = train)
plot(decision)
text(decision)
partition.tree(decision)

####FOREST###
forest <- randomForest(Default ~ ., data = train, nodesize=5, ntree = 500, mtry = 4)
predictions_forest <- predict(object = forest, newdata = test[-17], type = "prob")
default_probabilities <- predictions_forest[,2]  
binary_forest <- as.factor(ifelse(default_probabilities > 0.5, 1, 0))

head(predictions_forest, n = 30)

head(binary_forest, n = 30)

###FORESTMATRIX####
binary_forest <- as.factor(binary_forest)
test$Default <- as.factor(test$Default)
set.seed(1234)
confusionMatrix(data = binary_forest, reference = test$Default)


