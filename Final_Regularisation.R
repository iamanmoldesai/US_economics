#considering inflation as objective variable


library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

economics <- read.csv((file.choose()))

glimpse(economics)

#random seed
set.seed(100)

#index for randomly sampling
index = sample(1:nrow(economics), 0.66*nrow(economics))


#creating the training and validation set
training <- economics[index,]
validating <- economics[-index,]

#training set contains 66 percent of the data
#while the validation set contains the remaining 34 percent

dim(training)
dim(validating)

#Linear Regression

linreg = lm(uempmed ~ pce + pop + psavert + unemploy, data = training)
summary(linreg)

#Mod Evaluation Metrics

#Step 1 - create the evaluation metrics function

cal_eval_metrics = function(mod, df, preds, objective){
  resids1 = df[,objective] - preds
  resids2 = resids1**2
  L = length(preds)
  rsq = as.character(round(summary(mod)$r.squared, 2))
  adj_rsq = as.character(round(summary(mod)$adj.r.squared, 2))
  print(adj_rsq) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/L), 2))) #RMSE
}

# Step 2 - predicting and evaluating the mod on training data
preds = predict(linreg, newdata = training)
cal_eval_metrics(linreg, training, preds, objective = 'uempmed')

# Step 3 - predicting and evaluating the mod on validation data
preds = predict(linreg, newdata = validating)
cal_eval_metrics(linreg, validating, preds, objective = 'uempmed')

# Computing RMSE and R^2
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Mod performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

#Regularization

reg_column = c('pce', 'pop', 'psavert', 'uempmed', 'unemploy')

dummy <- dummyVars(uempmed ~ ., data = economics[,reg_column])

training_dummy = predict(dummy, newdata = training[,reg_column])

validating_dummy = predict(dummy, newdata = validating[,reg_column])

print(dim(training_dummy));

print(dim(validating_dummy))

#creating matrix

a = as.matrix(training_dummy)
b_training = training$uempmed

a_validating = as.matrix(validating_dummy)
b_validating = validating$uempmed

#Lasso Regression

lambdas_value <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(a, b_training, alpha = 1, lambda = lambdas_value, standardize = TRUE, nfolds = 5)

#Plot for cross validation results
plot(lasso_reg)

# Best 
best_lambda <- lasso_reg$lambda.min 
best_lambda

lasso_mod <- glmnet(a, b_training, alpha = 1, lambda = best_lambda, standardize = TRUE)

#Each line shows coefficients for one variables, for different lambdas.
#The higher the lambda, the more the coefficients are shrinked towards zero.

training_hat_cv_ls <- predict(lasso_mod, a)
ssr_cv <- t(b_training - training_hat_cv_ls) %*% (b_training - training_hat_cv_ls)
rsq_lasso_cv <- cor(b_training, training_hat_cv_ls)^2
res <- glmnet(a, b_training, alpha = 1, lambda = lambdas_value, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(a), cex = .7)


preds_training <- predict(lasso_mod, s = best_lambda, newx = a)
eval_results(b_training, preds_training, training)

preds_validating <- predict(lasso_mod, s = best_lambda, newx = a_validating)
eval_results(b_validating, preds_validating, validating)




#Ridge Regression

lambdas_value <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(a, b_training, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas_value)

summary(ridge_reg)


cv_ridge <- cv.glmnet(a, b_training, alpha = 0, lambda = lambdas_value)
opt_lambda <- cv_ridge$lambda.min
opt_lambda

plot(cv_ridge)
#chart for Ridge

training_hat_cv <- predict(cv_ridge, a)
ssrr_cv <- t(b_training - training_hat_cv) %*% (b_training - training_hat_cv)
rsq_ridge_cv <- cor(b_training, training_hat_cv)^2
ress <- glmnet(a, b_training, alpha = 0, lambda = lambdas_value, standardize = FALSE)
plot(ress, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(a), cex = .4)






# Making prediction and evaluation on training data
preds_training <- predict(ridge_reg, s = opt_lambda, newx = a)
eval_results(b_training, preds_training, training)

# Making prediction and evaluation on validation data
preds_validating <- predict(ridge_reg, s = opt_lambda, newx = a_validating)
eval_results(b_validating, preds_validating, validating)

#Elastic Net Regression

# Set training control
training_cont <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5,
                              search = "random",
                              verboseIter = TRUE)

# Training the model
elastic_reg <- train(uempmed ~ pce + pop + psavert + unemploy,
                     data = training,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = training_cont)


# Best tuning parameters
elastic_reg$bestTune


# Making predictions on training set
preds_training <- predict(elastic_reg, a)
eval_results(b_training, preds_training, training) 

# Making predictions on validation set
preds_validating <- predict(elastic_reg, a_validating)
eval_results(b_validating, preds_validating, validating)



