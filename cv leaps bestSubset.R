library(caret)
library(leaps)
library(tidyverse)

data <- read.csv(file="states.csv",header = TRUE)
data2 <- data[,-1]

nr <- nrow(data2)

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

trainSetIndex <- (sample(1:(nr),(nr)*.8))
testSetIndex <- c(1:nr)[(1:nr) %in% c(trainSetIndex)==FALSE]

set.train <- data2[trainSetIndex, ]
set.test <- data2[testSetIndex,]

#normalization
trainParam <- caret::preProcess(as.matrix(set.train))

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


models <- regsubsets(as.formula(paste0(colnames(set.train)[1], "~.")), data = set.train, nvmax = ncol(set.train)-1)

res.sum <- summary(models)

data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 10)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
# Compute cross-validation error

#get_model_formula(3, models, "Poverty")

model.ids <- 1:(ncol(set.train)-1)

cv.errors <-  map(model.ids, get_model_formula, models, "Poverty") %>%
  map(get_cv_error, data = data) %>%
  unlist()

cv.errors

coef(models, which.min(cv.errors))

finalNames <- names(coef(models, which.min(cv.errors)))[-1]

f <- as.formula(
  paste(colnames(set.train[1]), 
        paste(finalNames, collapse = " + "), 
        sep = " ~ "))

model <- lm(f,set.train)
summary(model)

plot(model$fitted.values,set.train[,1])
abline(lm(model$fitted.values~set.train[,1]))
cor(set.train[,1],model$fitted.values)

predictions <- predict(model,set.test)
RMSE(predictions,set.test[,1])
MAPE(set.test[,1],predictions)

plot(set.test[,1],predictions)
abline(lm(set.test[,1]~predictions))
cor(set.test[,1],predictions)
