
model <- 
  xgboost(data = as.matrix(set.train[,-1]), 
          label = as.matrix(set.train[,1,drop=FALSE]),
          booster = "gblinear", 
          objective = "reg:squarederror", 
          max.depth = 10, 
          nround = 100, 
          lambda = bestLambda, 
          lambda_bias = 0, 
          alpha = bestAlpha)

model$niter

model2 <- xgboost::xgb.cv(data = as.matrix(set.train[,-1]), 
                          nfold=numFolds,
                          label = as.matrix(set.train[,1,drop=FALSE]),
                          booster = "gblinear", 
                          objective = "reg:squarederror", 
                          max.depth = 10, 
                          nround = 100, 
                          lambda = bestLambda, 
                          lambda_bias = 0, 
                          alpha = bestAlpha)

mat <- xgb.importance (feature_names = colnames(set.train[,-1]),model = model2)
xgb.plot.importance (importance_matrix = mat[1:ncol(set.train[,-1])]) 

source("shap.R")
shap_result = shap.score.rank(xgb_model = model2, 
                              X_train =set.train[,-1],
                              shap_approx = F
)

# `shap_approx` comes from `approxcontrib` from xgboost documentation. 
# Faster but less accurate if true. Read more: help(xgboost)

## Plot var importance based on SHAP
var_importance(shap_result, top_n=ncol(X))

## Prepare data for top N variables
shap_long = shap.prep(shap = shap_result,
                      X_train = X , 
                      top_n = ncol(X)
)

## Plot shap overall metrics
plot.shap.summary(data_long = shap_long)


## 
xgb.plot.shap(data = X, # input data
              model = model, # xgboost model
              features = names(shap_result$mean_shap_score[1:ncol(X)]), # only top 10 var
              n_col = 3, # layout option
              plot_loess = T # add red line to plot
)

y_pred <- predict(model, as.matrix(set.test[,-1]))

plot(set.test[,1],y_pred)
abline(lm(set.test[,1]~y_pred))

MAPE(y_pred,set.test[,1])
RMSE(y_pred,set.test[,1])

