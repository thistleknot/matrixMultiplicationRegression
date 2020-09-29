#elastic Net

#example: https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net
#https://www.youtube.com/watch?v=ctmNq7FgbvI

# Load libraries, get data & set seed for reproducibility ---------------------
# seef for reproducibility
library(glmnet)  # for ridge regression
library(dplyr)   # for data cleaning
library(psych)   # for function tr() to compute trace of a matrix
library(parallel)

preData <- read.csv(file="states.csv", header=T)
data <- preData[,c(-1)]

# Center y, X will be standardized in the modeling function
Y <- data %>% select(Poverty) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
X <- data %>% select(-Poverty) %>% as.matrix()

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

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

rownames(finalResults)


