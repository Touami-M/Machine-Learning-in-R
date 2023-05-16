install.packages("adabag")
library(adabag)
library(mlbench)
library(e1071)

# Load the shuttle dataset
data = Shuttle
summary(data)

# Split the data into a training set and a test set
trainIndex <- sample(1:nrow(data), 5000)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]


# Train the AdaBoost model with full dataset
shuttle.adaboost <- boosting(Class~., data=data, boos=TRUE, mfinal=3)
shuttle.adaboost

# Train the AdaBoost model with traindataset
adaModel <- boosting(Class~., data=trainData, boos=TRUE, mfinal=3)
adaModel

# Make predictions on the test set
predictions <- predict.boosting(adaModel, newdata = testData)
predictions

# tune the model 
# Identify the hyperparameters that you want to tune
hyperparameters <- list(mfinal = c(1, 2, 3, 4, 5),
                        nu = c(0.1, 0.2, 0.3, 0.4, 0.5))

# Create a grid of values for the hyperparameters that you want to tune
grid <- expand.grid(hyperparameters)

# Train the model for each combination of hyperparameters in the grid
models <- lapply(1:nrow(grid), function(i) {
  model <- boosting(Class~., data=trainData, boos=TRUE, mfinal=grid$mfinal[i], nu=grid$nu[i])
  model
})

# Evaluate the performance of each model on the test set
accuracies <- sapply(models, function(model) {
  predictions <- predict.boosting(model, newdata = testData)
  mean(predictions == testData$Class)
})

# Select the model with the best performance on the test set
best_model <- models[[which.max(accuracies)]]

# Print the best model
best_model


