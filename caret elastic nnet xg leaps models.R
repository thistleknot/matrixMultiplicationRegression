library(matlib)
library(caret)
library(glmnet)
library(ModelMetrics)

options(digits = 4)
normalizeResponse <- "Y"

data <- read.csv(file="states.csv",header = TRUE)
#data2 <- data[,-1]

seed <- sample(1:100,1)

nr <- nrow(data3)
nc <- ncol(data3)

trainSetIndex <- (sample(1:(nr),(nr)*.8))
testSetIndex <- c(1:nr)[(1:nr) %in% c(trainSetIndex)==FALSE]

print(seed)
set.seed(seed)

lapply(1:(ncol(data)-1),function(y)
{#y=2
        data2 <- data[,-1]
        print(colnames(data2[,y,drop=FALSE]))
        data2 <- cbind(data2[,y,drop=FALSE],data2[,-y,drop=FALSE])
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
        
        invisible(capture.output(suppressMessages(suppressWarnings(xg <- train(f, data=set.train, method="xgbLinear", metric="RMSE", trControl=train.control, tuneLength = numFolds)))))
        
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
        
        invisible(capture.output(suppressMessages(suppressWarnings(model_nnet <- train(f, data = set.train,method = "nnet", trControl = train.control)))))
        
        plot(model_nnet)
        
        best_caret <- train(f, data = set.train,method = "leapBackward", trControl = train.control)
        
        #best_caret$finalModel
        
        best_caret_model <- lm(cbind(set.train[,1,drop=FALSE],set.train[,gsub("\\`","",names(coef(best_caret$finalModel, best_caret$bestTune[,1])))[-1],drop=FALSE]))
        summary(best_caret_model)
        
        predict_best_caret <- predict(best_caret_model,set.test[,-1,drop=FALSE])
        
        model_enet <- glmnet(data.matrix(set.train[,-1,drop=FALSE]), as.matrix(set.train[,1,drop=FALSE]), alpha = enet$finalModel$tuneValue$fraction, lambda=enet$finalModel$lambda, standardize = FALSE, family = "gaussian", type.measure="deviance")
        
        plot(coef(model_enet))
        
        predict_glm <- predict.glmnet(model_glm,data.matrix(set.test[,-1,drop=FALSE]),lambda=model_enet$finalModel$tuneValue$lambda,s0=model_enet$finalModel$tuneValue$fraction)
        
        model_xg <- glmnet(data.matrix(set.train[,-1,drop=FALSE]), as.matrix(set.train[,1,drop=FALSE]), alpha = xg$finalModel$tuneValue$alpha, lambda=xg$finalModel$lambda, standardize = FALSE, family = "gaussian", type.measure="deviance")
        
        plot(coef(model_xg)[,ncol(coef(model_xg))])
        
        predict_xg <- predict.glmnet(model_xg,data.matrix(set.test[,-1,drop=FALSE]), alpha = xg$finalModel$tuneValue$alpha, lambda=xg$finalModel$lambda)
        predict_xg <- predict_xg[,ncol(predict_xg)]
        predict_nnet <- predict(model_nnet, set.test[,-1])
        
        predict_enet <- predict(model_enet,as.matrix(set.test[,-1]))
        
        #same as best parm's
        #predict_xg <- predict(model_xg,set.test[,-1])
        
        predict_sets <- list(predict_enet,predict_glm,predict_xg,predict_best_caret,predict_nnet)
        
        rmses <- lapply(1:length(predict_sets), function(x)
        {
                
                plot((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predict_sets[[x]]* trainParam$std[1]) + trainParam$mean[1])
                return <- rmse((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predict_sets[[x]]* trainParam$std[1]) + trainParam$mean[1])
        })
        print(unlist(rmses))
        print(summary(best_caret_model))
        
})
