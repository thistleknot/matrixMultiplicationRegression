library(matlib)
library(caret)
library(glmnet)
library(ModelMetrics)

options(digits = 3)

data <- read.csv(file="states.csv",header = TRUE)
data2 <- data[,-1]

subsetNames <- colnames(data2[,-1])

#derive interactions here
combinations <- unique(combn(subsetNames, 2,simplify = FALSE))

as.double(unlist(data2[,"Income",drop=FALSE]))*as.double(unlist(data2[,"Population",drop=FALSE]))

#temp <- combinations(length(subsetNames), 2, subsetNames, repeats.allowed = FALSE)
interactions <- do.call(cbind,(lapply(combinations, function (b)
{#b=combinations[[1]]
        interactions <- data.frame(as.double(unlist(data2[,unlist(b)[1]]))*as.double(unlist(data2[,unlist(b)[2]])))
        colnames(interactions) <- paste0(unlist(b)[1],"*",unlist(b)[2])
        return(interactions)
        
})))

squared <- do.call(cbind,lapply(subsetNames, function(s)
{#s=subsetNames[1]
        sqr <- data2[,s,drop=FALSE]^2
        colnames(sqr) <- paste0(s,"^2")
        return(sqr)
}))

data3 <- cbind(data2,interactions,squared)

nr <- nrow(data3)
nc <- ncol(data3)

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

trainSetIndex <- (sample(1:(nr),(nr)*.8))
testSetIndex <- c(1:nr)[(1:nr) %in% c(trainSetIndex)==FALSE]

normalizeResponse <- "Y"

set.train <- data3[trainSetIndex, ]
set.test <- data3[testSetIndex,]

if(normalizeResponse=="Y")
{
        trainParam <- caret::preProcess(as.matrix(set.train))
        
        set.train <- predict(trainParam, set.train)
        
        set.test <- predict(trainParam, set.test)
}

numFolds = 3

f <- as.formula(paste0(colnames(set.train)[1], "~."))

train.control <- trainControl(method = "cv", number = numFolds, allowParallel = TRUE, search = "grid")

xg <- train(f, data=set.train, method="xgbLinear", metric="RMSE", trControl=train.control, tuneLength = numFolds)

plot(xg)

xg$finalModel$tuneValue

enet <- train(f, data=set.train, method="enet", trControl=train.control)

enet$finalModel$tuneValue

plot(enet)


#coef(xg$finalModel,s=xg$finalModel$tuneValue$lambda)
#coef(enet$finalModel,enet$bestTune[,1])

#really bad values
model_glm <- glmnet(data.matrix(set.train[,-1,drop=FALSE]), as.matrix(set.train[,1,drop=FALSE]), alpha = enet$finalModel$tuneValue$fraction, lambda=enet$finalModel$lambda, standardize = FALSE, family = "gaussian", type.measure="deviance")
#predicts <- predict.glmnet(model,data.matrix(set.test[,-1,drop=FALSE]),alpha = enet$finalModel$tuneValue$fraction, lambda=enet$finalModel$lambda)

best_caret <- train(f, data = set.train,method = "leapBackward", trControl = train.control)

best_caret$finalModel

best_caret_model <- lm(cbind(set.train[,1,drop=FALSE],set.train[,names(coef(best_caret$finalModel, best_caret$bestTune[,1]))[-1]]))
summary(best_caret_model)

predict_best_caret <- predict(best_caret_model,set.test[,-1,drop=FALSE])

model_enet <- glmnet(data.matrix(set.train[,-1,drop=FALSE]), as.matrix(set.train[,1,drop=FALSE]), alpha = enet$finalModel$tuneValue$fraction, lambda=enet$finalModel$lambda, standardize = FALSE, family = "gaussian", type.measure="deviance")

plot(coef(model_enet))

predict_glm <- predict.glmnet(model_glm,data.matrix(set.test[,-1,drop=FALSE]),lambda=model_enet$finalModel$tuneValue$lambda,s0=model_enet$finalModel$tuneValue$fraction)

model_xg <- glmnet(data.matrix(set.train[,-1,drop=FALSE]), as.matrix(set.train[,1,drop=FALSE]), alpha = xg$finalModel$tuneValue$alpha, lambda=xg$finalModel$lambda, standardize = FALSE, family = "gaussian", type.measure="deviance")

plot(coef(model_xg)[,ncol(coef(model_xg))])

predict_xg <- predict.glmnet(model_xg,data.matrix(set.test[,-1,drop=FALSE]), alpha = xg$finalModel$tuneValue$alpha, lambda=xg$finalModel$lambda)
predict_xg <- predict_xg[,ncol(predict_xg)]

predict_enet <- predict(model_enet,as.matrix(set.test[,-1]))

#same as best parm's
#predict_xg <- predict(model_xg,set.test[,-1])

rmse((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predict_enet* trainParam$std[1]) + trainParam$mean[1])
rmse((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predict_glm* trainParam$std[1]) + trainParam$mean[1])
rmse((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predict_xg* trainParam$std[1]) + trainParam$mean[1])
rmse((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predict_best_caret* trainParam$std[1]) + trainParam$mean[1])

#exact same
plot((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predict_enet* trainParam$std[1]) + trainParam$mean[1])
#***
plot((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predict_glm* trainParam$std[1]) + trainParam$mean[1])
plot((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predict_xg* trainParam$std[1]) + trainParam$mean[1])
plot((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predict_best_caret* trainParam$std[1]) + trainParam$mean[1])



#cor(tested,predictions)




