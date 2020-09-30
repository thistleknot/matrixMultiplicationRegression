library(matlib)
library(xgboost)
options(digits = 3)

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

numFolds = 10

folds=sample(rep(0:numFolds, length=nrow(set.train)))

#training portion (derive lambda's)
list.of.fits <- list()
for (i in 0:numFolds) {
        #i=1
        X <- as.matrix(set.train[folds!=i,-1,drop=FALSE])
        Y <- as.matrix(set.train[folds!=i,1,drop=FALSE])
        
        fit.name <- paste0("alpha", i/numFolds)
        
        list.of.fits[[fit.name]] <-
                cv.glmnet(X, Y, type.measure="mse", alpha=i/numFolds,
                          family="gaussian")
}

#View(list.of.fits)

results <- data.frame()

#validation portion, derive alpha's
for(i in 0:numFolds)
{#i=0
        objTest <- as.matrix(set.train[folds==i,])
        
        fit.name <- paste0("alpha", i/numFolds)
        
        predicted <- predict(list.of.fits[[fit.name]],
                             s=list.of.fits[[fit.name]]$lambda.min, newx=objTest[,-1])
        
        mse <- mean((objTest[,1] - predicted)^2)
        
        temp <- data.frame(alpha=i/numFolds, mse=mse, fit.name=fit.name)
        results <- rbind(results, temp)
}

bestAlpha <- results$alpha[which(results$mse == min(results$mse))]

print(bestAlpha)

bestLambda <- cv.glmnet(X, Y, type.measure="mse", alpha=i/numFolds,
                        family="gaussian")$lambda.min
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

model <- lm(f,set.train)

summary(model)

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

