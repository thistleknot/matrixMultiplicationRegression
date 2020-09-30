#elastic Net

#example: https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net
#https://www.youtube.com/watch?v=ctmNq7FgbvI

# Load libraries, get data & set seed for reproducibility ---------------------
# seef for reproducibility
library(glmnet)  # for ridge regression
library(dplyr)   # for data cleaning
library(psych)   # for function tr() to compute trace of a matrix
library(parallel)
library(MLmetrics)
library(pls)
library(caret)
library(leaps)
library(tidyverse)

preData <- read.csv(file="states.csv", header=T)
data <- preData[,c(-1)]

# Center y, X will be standardized in the modeling function
Y <- data %>% select(Poverty) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
X <- data %>% select(-Poverty) %>% as.matrix()

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

f <- as.formula(
  paste(colnames(Y), 
        paste(colnames(X[,-1]), collapse = " + "), 
        sep = " ~ "))

pls.model <- plsr(f, data = data, validation= "CV")

# Find the number of dimensions with lowest cross validation error
cv = RMSEP(pls.model)
best.dims = which.min(cv$val[estimate = "adjCV", , ]) - 1

# Rerun the model
pls.model = plsr(f, data = data, ncomp = best.dims)

coefficients = names(round(coef(pls.model)[,1,1],2))

finalResults <- round(coef(pls.model)[,1,1],2)

finalNames <- names(finalResults[abs(finalResults)>0])

f <- as.formula(
  paste(colnames(Y), 
        paste(finalNames, collapse = " + "), 
        sep = " ~ "))

model <- lm(f,data)

summary(model)

#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/155-best-subsets-regression-essentials-in-r/#:~:text=The%20best%20subsets%20regression%20is,best%20subsets%20regression%20using%20R.
# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

models <- regsubsets(Poverty~., data = data, nvmax = 5)
res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)
get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
# Compute cross-validation error
model.ids <- 1:5

#get_model_formula(3, models, "Poverty")

model.ids <- 1:5
cv.errors <-  map(model.ids, get_model_formula, models, "Poverty") %>%
  map(get_cv_error, data = data) %>%
  unlist()

cv.errors

coef(models, which.min(cv.errors))

finalNames <- names(coef(models, which.min(cv.errors)))[-1]

f <- as.formula(
  paste(colnames(Y), 
        paste(finalNames, collapse = " + "), 
        sep = " ~ "))

model <- lm(f,data)

summary(model)

plot(model$fitted.values,model$model$Poverty)
abline(lm(model$fitted.values~model$model$Poverty))


#train_rows <- sample(1:nrow(data), .66*nrow(data))
#x.train <- X[train_rows,]
#x.test <- X[-train_rows,]

#y.train <- Y[train_rows,]
#y.test <- Y[-train_rows,]

#normally uses train
list.of.fits <- list()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  list.of.fits[[fit.name]] <-
    cv.glmnet(X, Y, type.measure="mse", alpha=i/10,
              family="gaussian")
}

results <- data.frame()

#x is normally x.test and y.test
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  predicted <- predict(list.of.fits[[fit.name]],
                       s=list.of.fits[[fit.name]]$lambda.1se, newx=X)
  
  mse <- mean((Y - predicted)^2)
  
  temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results <- rbind(results, temp)
                       
}

bestAlpha <- results$alpha[which(results$mse == min(results$mse))]
print(bestAlpha)
bestLambda <- cv.glmnet(X, Y, type.measure="mse", alpha=i/10,
                       family="gaussian")$lambda.1se
print(bestLambda)

bestModel <- glmnet(X, Y, alpha = bestAlpha, lambda=bestLambda, standardize = FALSE, family = "gaussian", type.measure="deviance")

results2 <- cbind(rownames(bestModel$beta),round(abs(bestModel$beta)[,1],3))

results2[order(results2[,2],decreasing=TRUE),]

finalResults <- results2[order(results2[,2],decreasing=TRUE),][results2[order(results2[,2],decreasing=TRUE),2]>0,]

finalNames <- rownames(finalResults)

formula = paste(colnames(Y)~finalNames)

f <- as.formula(
  paste(colnames(Y), 
        paste(finalNames, collapse = " + "), 
        sep = " ~ "))

model <- lm(f,data)

summary(model)
