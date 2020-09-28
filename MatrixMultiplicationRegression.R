
library(matlib)
options(digits = 3)

preData <- read.csv(file="states.csv", header=T)
data <- preData[,c(-1)]

X <- as.matrix(cbind(data.frame(matrix(1,nrow(data))),data[,-1]))
Y <- as.matrix(data[,1,drop=FALSE])

XT <- t(X)

#https://stackoverflow.com/questions/9894068/multiplying-two-matrices-in-r
#tcrossprod(t(X))

#MMULT
XTxX <- t(X) %*% t(XT)

#minverse <- Ginv(XTxX,fractions=TRUE,10^17)
covMatrix <- inv(XTxX,fractions=TRUE,10^17)
#minverse <- 

minverse_x_XTxX <- t(t(XT) %*% t(covMatrix))
#View(minverse_x_XTxX)

#used for leverage
FinalMatrix <- t((X) %*% (minverse_x_XTxX))

corMatrix <- cor(data)

invCorMatrix <- inv(corMatrix)

#used for R^2
someValue <- diag(invCorMatrix)[1]
someValue2 <- 1/someValue

#doesn't matter where Y is
RSqr <- 1-someValue2

MRsqr <- sqrt(RSqr)

someValue3 <- (1-RSqr)*(nrow(data)-1)
DF <- nrow(data)-ncol(data)
AdjustedR <- 1-(someValue3/DF)

#MMULT(MMULT(MINVERSE(MMULT(TRANSPOSE(X), X)),TRANSPOSE(X)),Y)
coeff <- minverse_x_XTxX %*% Y

yhat <- X%*%coeff

residuals <- Y-yhat
residuals2 <- residuals^2
RSS <- sum(residuals2)
stdErr <- sqrt(RSS/DF)

#https://stats.stackexchange.com/questions/166533/how-exactly-are-standardized-residuals-calculated
stdRes <- residuals/sqrt(1/(nrow(data)-1)*RSS)

#STD_ERR*SQRT(INDEX(DIAG(COVMATRIX), I60))
stdErrs <- stdErr* sqrt( diag(covMatrix))
tScores <- coeff/stdErrs
pScores <- pt(abs(tScores)*-1, DF) *2

leverage <- diag(FinalMatrix)

dataWLeverage <- cbind(preData,yhat,residuals,stdErrs,leverage)
colnames(dataWLeverage) <- c(colnames(preData),"yhat","residuals","stdErrs","leverage")
plot(dataWLeverage[,"Poverty"],dataWLeverage[,"leverage"])

cterms <- (cbind(coeff,tScores,pScores))
rownames(cterms) <- c("constant",colnames(data)[-1])
colnames(cterms) <- c("coeff","tScores","pScores")
print(cterms)
print(round(corMatrix,2))
vars <- c(DF,RSqr, MRsqr, AdjustedR,RSS,stdErr)
names(vars) <- c("DF", "RQsr", "MRsqr", "AdjustedR", "RSS", "stdErr")
print(vars)

print(dataWLeverage)

#next is studentized residuals
#cook's distance