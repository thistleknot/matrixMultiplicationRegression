source("functions.R")

#knn.reg.bestK(x_training, x_test, y_training, y_test)

set.train2 <- set.train

dataset_knn <- back_step_partial_correlation_knn(set.train2)

dataset_reduced <- rmse_backstep_knn(dataset_knn)
knn_predictors <- read.csv(text=paste (dataset_reduced[[2]][[1]], collapse=","),header = FALSE)

#knn_predictors <- colnames(dataset_knn)

set.train2 <- set.train[,c(unlist(knn_predictors))]

corPlot(PCOR(set.train2))
knn_model <- knn.reg.bestK(set.train2)

#knn_model$k.opt

#predict on test data
yhat = knn.reg(set.train[,-1,drop=FALSE], test=set.test[,colnames(set.train)][,-1,drop=FALSE], unlist(set.train[,1,drop=FALSE]), knn_model$k.opt)
yhatFull = knn.reg(rbind(set.train,set.test)[,colnames(set.train2)][,-1,drop=FALSE], test=rbind(set.train,set.test)[,colnames(set.train2)][,-1,drop=FALSE], unlist(rbind(set.train,set.test)[,colnames(set.train2)][,1,drop=FALSE]), knn_model$k.opt)
#yhat$R2Pred


#predict on self data with best knn
RMSE(yhat$pred,unlist(set.test[,1]))
MAPE(yhat$pred,unlist(set.test[,1]))
plot(yhat$pred,unlist(set.test[,1]))

RMSE(yhatFull$pred,unlist(rbind(set.train,set.test)[,1]))
MAPE(yhatFull$pred,unlist(rbind(set.train,set.test)[,1]))
plot(yhatFull$pred,unlist(rbind(set.train,set.test)[,1]))

#resid <- unlist(set.final[,1])-yhatFull$pred

library(dbscan)
kNNdistplot(rbind(set.train,set.test)[,colnames(set.train2)], k=knn_model$k.opt)

eps <- median(kNNdist(rbind(set.train,set.test)[,colnames(set.train2)], k=knn_model$k.opt))

cl <- dbscan(rbind(set.train,set.test)[,colnames(set.train2)], eps = eps, minPts = knn_model$k.opt)
pairs(rbind(set.train,set.test)[,colnames(set.train2)], col = cl$cluster+1L)
