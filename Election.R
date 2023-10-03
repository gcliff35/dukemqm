trainindexsplit <- createDataPartition(electionDataTrain$Obama_margin_percent, p = 0.8, list = FALSE)
trainsplit <- electionDataTrain[trainindexsplit, ] 
testsplit <- electionDataTrain[-trainindexsplit, ]


x <- model.matrix(Obama_margin_percent ~ . - 1, trainsplit)
y <- trainsplit$Obama_margin_percent
lasso <- glmnet(x, y, alpha=1)
cv.lasso <- cv.glmnet(x, y, alpha=1)
best_lambda <- cv.lasso$lambda.min
coef(lasso, s=best_lambda)
prediction_lasso <- predict(lasso, newx=x, s=best_lambda)
plot(lasso)
plot(cv.lasso)

testmatrix <- model.matrix(Obama_margin_percent ~ . - 1, data=testsplit)
predictions <- predict(lasso, newx=testmatrix, s=best_lambda)
dim(x)
dim(testmatrix) 

# Create matrices for the predictors (excluding the dependent variable)
train_matrix <- model.matrix(Obama_margin_percent ~ . - 1, data=trainsplit)
test_matrix <- model.matrix(Obama_margin_percent ~ . - 1, data=testsplit)

# Create vectors for the dependent variable
train_y <- trainsplit$Obama_margin_percent
set.seed(123) # Setting a seed for reproducibility
cv.lasso <- cv.glmnet(train_matrix, train_y, alpha=1) # alpha=1 means Lasso regression

# Plot the cross-validation results
plot(cv.lasso)

best_lambda <- cv.lasso$lambda.min
predictions <- predict(cv.lasso, newx=test_matrix, s=best_lambda)

actuals <- testsplit$Obama_margin_percent
mse <- mean((predictions - actuals)^2)
print(mse)

coef(cv.lasso, s=best_lambda)

combined_data <- rbind(trainsplit, testsplit)
combined_matrix <- model.matrix(Obama_margin_percent ~ . - 1, data=combined_data)

train_rows <- nrow(trainsplit)
train_matrix <- combined_matrix[1:train_rows, ]
test_matrix <- combined_matrix[(train_rows + 1):nrow(combined_matrix), ]

set.seed(123) # Setting a seed for reproducibility
cv.lasso <- cv.glmnet(train_matrix, train_y, alpha=1) # alpha=1 means Lasso regression

# Plot the cross-validation results
plot(cv.lasso)

best_lambda <- cv.lasso$lambda.min
predictions <- predict(cv.lasso, newx=test_matrix, s=best_lambda)

naive <- lm(Obama_margin_percent ~ ., data = trainsplit)
summary(naive)

# Ensure that the levels of 'County' in testsplit match those in trainsplit
testsplit$County <- factor(testsplit$County, levels=levels(trainsplit$County))

predictions_naive <- predict(naive, newdata = testsplit)
predictions_naive


