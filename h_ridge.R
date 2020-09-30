#example: https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net

# Load libraries, get data & set seed for reproducibility ---------------------
set.seed(123)    # seef for reproducibility
library(glmnet)  # for ridge regression
library(dplyr)   # for data cleaning
library(psych)   # for function tr() to compute trace of a matrix
library(parallel)

preData <- read.csv(file="states.csv", header=T)
data <- preData[,c(-1)]

# Center y, X will be standardized in the modelling function
y <- data %>% select(Poverty) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
X <- data %>% select(-Poverty) %>% as.matrix()

# Perform 10-fold cross-validation to select lambda ---------------------------
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

plot(lambdas_to_try)

# Setting alpha = 0 implements ridge regression
ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(ridge_cv)

#2

# Best cross-validated lambda
lambda_cv <- ridge_cv$lambda.min
# Fit final model, get its sum of squared residuals and multiple R-squared
model_cv <- glmnet(X, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X)
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)
rsq_ridge_cv <- cor(y, y_hat_cv)^2

# Calculate the weights from univariate regressions
weights <- unlist(mclapply(seq(ncol(X)), function(predictor) {
  uni_model <- lm(y ~ X[, predictor])
  coeff_variance <- summary(uni_model)$coefficients[2, 2]^2
}))

# Heteroskedastic Ridge Regression loss function - to be minimized
hridge_loss <- function(betas) {
  sum((y - X %*% betas)^2) + lambda * sum(weights * betas^2)
}

# Heteroskedastic Ridge Regression function
hridge <- function(y, X, lambda, weights) {
  # Use regular ridge regression coefficient as initial values for optimization
  model_init <- glmnet(X, y, alpha = 0, lambda = lambda, standardize = FALSE)
  betas_init <- as.vector(model_init$beta)
  # Solve optimization problem to get coefficients
  coef <- optim(betas_init, hridge_loss)$par
  # Compute fitted values and multiple R-squared: matrix multiplication
  fitted <- X %*% coef
  h_ssr <- t(y - fitted) %*% (y - fitted)
  rsq <- cor(y, fitted)^2
  names(coef) <- colnames(X)
  output <- list("model" = model_init,
                  "coef" = coef,
                 "fitted" = fitted,
                 "rsq" = rsq,
                 "ssr" = h_ssr,
                 "lambda" = lambda)
  return(output)
}

# Fit model to the data for lambda = 0.001

h_ridge_cv <- mclapply(1:length(lambdas_to_try), function(x)
  {#x=1
  lambda = lambdas_to_try[x]
  
  hridge_model <- hridge(y, X, lambda, weights = weights)
  
  rsq_hridge <- hridge_model$rsq
  return(rsq_hridge)
  
})

bestLambda <- lambdas_to_try[which(unlist(h_ridge_cv)==min(unlist(h_ridge_cv)))]

plot(lambdas_to_try,unlist(h_ridge_cv))
#bestLambda <- unlist(mclapply(h_ridge_cv, `[[`, "lambda"))[which.min(unlist(mclapply(h_ridge_cv, `[[`, "ssr")))]

hridge_model <- hridge(y, X, bestLambda, weights = weights)

# Cross-validation or AIC/BIC can be employed to select some better lambda!
# You can find some useful functions for this at https://github.com/MichalOleszak/momisc/blob/master/R/hridge.R