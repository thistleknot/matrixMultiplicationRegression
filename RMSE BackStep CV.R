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
rownames(data) <- data[,1]
data2 <- data[,-1]
rownames(data2) <- data[,1]

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
  trainParam <- caret::preProcess(as.matrix(set.train),method=c("BoxCox", "center", "scale"))
  
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
diagnostic_plots(pca.model, data)
diagnostic_plots(model, data)

finalModel <- lm(f,data2)
summary(finalModel)

#using prior derived PCA's, prior best.dim's
finalModelPCA <- lm(cbind(data2[,1],data.frame(predict(set.train.pca, newdata=predict(trainParam, data)))[,1:best.dims]))
summary(finalModelPCA)

diagnostic_plots(finalModelPCA,data)

hist(finalModel$residuals)

#using newly derived PCA's, prior best.dim's (not sure if I should re-derive, but the point is to find the best hparm's on a subset)
set.final <- predict(trainParam,data2)[,unlist(predictors)]
set.final.pca <- prcomp(set.final[,-1])
newdat<-set.final.pca$x[,1:best.dims]

finalModelPCA <- lm(cbind(set.final[,1,drop=FALSE],newdat))
summary(finalModelPCA)

library(gtools)

set.final$Groups = quantcut(set.final[,1],3)
cname <- colnames(data2[,1,drop=FALSE])
levels(set.final$Groups) <- c(paste("Low",cname),paste("Medium",cname),paste("High",cname))

g <- ggbiplot(set.final.pca, obs.scale = 1, var.scale = 1, 
              labels=data[rownames(set.final),1],
              groups = set.final$Groups,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

library(pca3d)
options(rgl.printRglwidget = TRUE)

library(rgl)
bg3d("white")

test <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

pca3d(set.final.pca,group=set.final$Groups , show.scale=TRUE, show.plane = FALSE, show.labels = test ,show.centroids = TRUE,show.ellipses=FALSE, show.axe.titles = TRUE, show.group.labels=TRUE, biplot=TRUE)

rglwidget()

data[order(data$Poverty),]