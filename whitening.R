library(whitening)
library(DMwR)

source("functions.R")

data <- read.csv(file="states.csv",header = TRUE)
rownames(data) <- data[,1]
data2 <- data[,-1]
rownames(data2) <- data[,1]

cov(data2)
covMatrix(data2)

#scaledData (important to scale, else results don't converge on whiten)
sd <- scale(data2,center=TRUE,scale=TRUE)

#covariance Matrix
cm <- covMatrix(sd)
cm
#same as cov(sd)
cov(sd)

e <- eigen(covMatrix(scale(data2,center=TRUE,scale=TRUE)))

evecs <- e$vectors
evals <- e$values

#https://cbrnr.github.io/2018/12/17/whitening-pca-zca/

#whiten Matrix (same as whiteningMatrix)
wm <- evecs %*% diag(evals^(-1/2)) %*% t(evecs)
wm
whitening::whiteningMatrix(cm, method=c("ZCA"))

#wd <- tcrossprod(as.matrix(scale(data2),center=TRUE), wm)
#whitened data
wd <- as.matrix(sd) %*% wm
wd
whiten(as.matrix(sd), method=c("ZCA"))

#walk back

#scaled <- tcrossprod(wd,inv(wm))
scaled <- wd %*% t(inv(wm))

#apply mean and original sdev
unscale(scaled,sd)


df <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(data2[,-1]),method=c("ZCA"),center=TRUE))
colnames(df) <- c(colnames(data2[,1,drop=FALSE]),colnames(data2[,-1]))
model <- lm(data.frame(df))
summary(model)


df <- dplyr::select(data2[,-1], -c('Infant.Mort'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
summary(model)

df <- dplyr::select(data2[,-1], -c('Infant.Mort','University'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
sm <- summary(model)
round(sm$coefficients[,1],3)

#model goes down
df <- dplyr::select(data2[,-1], -c('Infant.Mort','University','Doctors'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
sm <- summary(model)
sm
round(sm$coefficients[,1],3)

#model goes down, but all significant terms
df <- dplyr::select(data2[,-1], -c('Infant.Mort','University','Doctors','Traf.Deaths'))
df2 <- cbind(scale(data2[,1],center=TRUE,scale=TRUE),whiten(as.matrix(df),method=c("ZCA"),center=TRUE))
colnames(df2) <- c(colnames(data2[,1,drop=FALSE]),colnames(df))
model <- lm(data.frame(df2))
sm <- summary(model)
sm
round(sm$coefficients[,1],3)

plot(model)
hist(model$residuals)



