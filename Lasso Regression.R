install.packages("glmnet")
install.packages("Matrix")
library(Matrix)
library(glmnet)
library(mlbench)
library(MASS)
data("BostonHousing")
dataset=BostonHousing #import dataset
BostonHousing$chas=NULL
x <- as.matrix(BostonHousing[, -14])
y <- BostonHousing$med
lasso_model <- glmnet(x, y, alpha = 1) 
print(lasso_model)
coefficients=coef(lasso_model)
plot(lasso_model, xvar = "lambda", label = TRUE)

# predict using the Lasso model
y_pred <- predict(lasso_model, newx = x)

# calculate the MSE and RMSE
mse <- mean((y - y_pred)^2)
rmse <- sqrt(mse)

# print the results
mse
rmse
