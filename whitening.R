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


