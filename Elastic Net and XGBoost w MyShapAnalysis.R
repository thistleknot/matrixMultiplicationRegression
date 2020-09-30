
library(matlib)
library(xgboost)
options(digits = 3)

preData <- read.csv(file="states.csv", header=T)
data <- preData[,c(-1)]

X <- as.matrix(cbind(data.frame(matrix(1,nrow(data))),data[,-1]))
Y <- as.matrix(data[,1,drop=FALSE])

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
                           top_n = ncol(X[,-1])
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


l <- length(names(shap_result$mean_shap_score))
#problem is, doesn't account for collinear terms
lapply(l:1, function(x)
        {
        f <- as.formula(
                paste(colnames(Y), 
                      paste(names(shap_result$mean_shap_score)[l:x], collapse = " + "), 
                      sep = " ~ "))
        finalNames <- names(shap_result$mean_shap_score)
        
        
        
        model <- lm(f,data)
        
        summary(model)
        
        
        
        
})

summary(lm(Poverty~Income+Crime+Population+Unemployed,data))