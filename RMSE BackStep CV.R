numFolds=10

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

library(car)
library(MASS)
library(data.table)

source("functions.R")

normalizeResponse <- "Y"

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

#normalization (not being used)
if(normalizeResponse=="Y")
{
  trainParam <- caret::preProcess(as.matrix(set.train))
  
  set.train <- predict(trainParam, set.train)
  
  set.test <- predict(trainParam, set.test)
}

set.train <- back_step_vif(set.train)

set.train <- back_step_partial_correlation(set.train)

result <- rmse_backstep(set.train)

predictors <- read.csv(text=paste (result[[2]][[1]], collapse=","),header = FALSE)

set.train <- set.train[,c(unlist(predictors))]

y <- set.train[,1,drop=FALSE]
x <- set.train[,-1,drop=FALSE]

f <- as.formula(paste(colnames(y), paste (colnames(x), collapse=" + "), sep=" ~ "))  # new formula

pls.options(parallel = makeCluster(4, type = "FORK"))

set.train.pcr <- pcr(f, ncol(set.train)-1, data = set.train, validation = "CV",segments = 10, segment.type = c("consecutive"))

# Find the number of dimensions with lowest cross validation error
cv = RMSEP(set.train.pcr)
best.dims = which.min(cv$val[estimate = "adjCV", , ]) - 1

# PCA, best.dims
#pls.model = plsr(f, data = set.train, ncomp = best.dims)

library(ggbiplot)
set.train.pca <- prcomp(set.train[,-1])

newdat<-set.train.pca$x[,1:best.dims]

summary(set.train.pca)

pca.model <- lm(cbind(set.train[,1,drop=FALSE],newdat))
summary(pca.model)

#library(pca3d)
library(gtools)
set.train$Groups = quantcut(set.train$Poverty,3)

g <- ggbiplot(set.train.pca, obs.scale = 1, var.scale = 1, 
              labels=data[rownames(set.train),1],
              groups = set.train$Groups,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

set.train <- dplyr::select(set.train, -c("Groups"))

model <- lm(f,set.train)
summary(model)

tested <- set.test[,1]

predictions <- predict(model,set.test)

predictions2 <- predict(pca.model, data.frame(predict(set.train.pca, newdata=set.test)))

if(normalizeResponse=="Y")
{
  predictions <- (predictions * trainParam$std[1]) + trainParam$mean[1]
  predictions2 <- (predictions2 * trainParam$std[1]) + trainParam$mean[1]
  tested <- (tested * trainParam$std[1]) + trainParam$mean[1]
}

RMSE(predictions,tested)
MAPE(predictions,tested)

RMSE(predictions2,tested)
MAPE(predictions2,tested)

plot(tested,predictions)
abline(lm(tested~predictions))
cor(tested,predictions)

plot(tested,predictions2)
abline(lm(tested~predictions2))
cor(tested,predictions2)

#new
if(TRUE)
{
  diagnostic_plots(pca.model, data)
  diagnostic_plots(model, data)
}

data[order(data$Poverty),]

finalModel <- lm(f,data2)
summary(finalModel)

finalModelPCA <- lm(cbind(data2[,1],data.frame(predict(set.train.pca, newdata=predict(trainParam, data)))[,1:best.dims]))
summary(finalModelPCA)

diagnostic_plots(finalModelPCA,data)

hist(finalModel$residuals)
