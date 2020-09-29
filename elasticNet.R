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

formula = paste(colnames(Y)~finalNames)

f <- as.formula(
  paste(colnames(Y), 
        paste(finalNames, collapse = " + "), 
        sep = " ~ "))

model <- lm(f,data)

summary(model)


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






library(matlib)
library(xgboost)
options(digits = 3)

preData <- read.csv(file="states.csv", header=T)
data <- preData[,c(-1)]

X <- as.matrix(cbind(data.frame(matrix(1,nrow(data))),data[,-1]))
Y <- as.matrix(data[,1,drop=FALSE])

model <- 
  xgboost(data = as.matrix(X)[,-1], 
          label = Y,
          booster = "gblinear", 
          objective = "reg:squarederror", 
          max.depth = 5, 
          nround = 25, 
          lambda = bestLambda, 
          lambda_bias = 0, 
          alpha = bestAlpha)

model$niter

mat <- xgb.importance (feature_names = colnames(X[,-1]),model = model)
xgb.plot.importance (importance_matrix = mat[1:ncol(X[,-1])]) 

source("shap.R")
shap_result = shap.score.rank(xgb_model = model, 
                              X_train =X[,-1],
                              shap_approx = F
)

# `shap_approx` comes from `approxcontrib` from xgboost documentation. 
# Faster but less accurate if true. Read more: help(xgboost)

## Plot var importance based on SHAP
var_importance(shap_result, top_n=ncol(X[,-1]))

## Prepare data for top N variables
shap_long = shap.prep(shap = shap_result,
                      X_train = X[,-1] , 
                      top_n = 9
)

## Plot shap overall metrics
plot.shap.summary(data_long = shap_long)


## 
xgb.plot.shap(data = X[,-1], # input data
              model = model, # xgboost model
              features = names(shap_result$mean_shap_score[1:ncol(X[,-1])]), # only top 10 var
              n_col = 3, # layout option
              plot_loess = T # add red line to plot
)

shap_result$mean_shap_score

summary(lm(Poverty~Infant.Mort,data))

summary(lm(Poverty~Infant.Mort+Income,data))

summary(lm(Poverty~Infant.Mort+Income+White,data))

summary(lm(Poverty~Infant.Mort+Income+White+University,data))

summary(lm(Poverty~Infant.Mort+Income+White+University+Crime,data))

summary(lm(Poverty~Infant.Mort+Income+White+University+Crime+Unemployed,data))

summary(lm(Poverty~Infant.Mort+Income+White+University+Crime+Unemployed+Doctors,data))

summary(lm(Poverty~Infant.Mort+Income+White+University+Crime+Unemployed+Doctors+Population,data))

