seed <- sample(1:100,1)
print(seed)
set.seed(seed)

library("car")
library("MASS")
library("data.table")
library("effects")
library("ModelMetrics")
library("MLmetrics")
library("parallel")
library("psych")
library("pls")
library("factoextra")
library("ggpubr")
library("gridExtra")
library("pca3d")
library("rgl")
library("DT")
library(ggbiplot)

source("functions.R")

normalizeResponse <- "Y"

data <- read.csv(file="states.csv",header = TRUE)
rownames(data) <- data[,1]
data2 <- data[,-1]
rownames(data2) <- data[,1]

nr <- nrow(data2)

trainSetIndex <- (sample(1:(nr),(nr)*.8))
testSetIndex <- c(1:nr)[(1:nr) %in% c(trainSetIndex)==FALSE]

set.train <- data2[trainSetIndex, ]
set.test <- data2[testSetIndex,]

#normalization (not being used)
if(normalizeResponse=="Y")
{
  #trainParam <- caret::preProcess(as.matrix(set.train),method=c("BoxCox", "center", "scale"))
  trainParam <- caret::preProcess(as.matrix(set.train),method=c("center", "scale"))
  
  set.train <- predict(trainParam, set.train)
  
  set.test <- predict(trainParam, set.test)
}

set.test2 <- set.test
set.train2 <- set.train

set.train2 <- back_step_vif_lm(set.train2)

set.train2 <- back_step_partial_correlation_lm(set.train2)

result <- rmse_backstep_lm(set.train2)

predictors <- read.csv(text=paste (result[[2]][[1]], collapse=","),header = FALSE)

set.train2 <- set.train2[,c(unlist(predictors))]

y <- set.train2[,1,drop=FALSE]
x <- set.train2[,-1,drop=FALSE]

f <- as.formula(paste(colnames(y), paste (colnames(x), collapse=" + "), sep=" ~ "))  # new formula

pls.options(parallel = makeCluster(4, type = "FORK"))

set.train2.pcr <- pcr(f, ncol(set.train2)-1, data = set.train2, validation = "CV",segments = 10, segment.type = c("consecutive"))

# Find the number of dimensions with lowest cross validation error
cv = RMSEP(set.train2.pcr)
best.dims = which.min(cv$val[estimate = "adjCV", , ]) - 1

# PCA, best.dims
#pls.model = plsr(f, data = set.train2, ncomp = best.dims)

set.train2.pca <- prcomp(set.train2[,-1,drop=FALSE])

PCADat<-cbind(set.train2[,1,drop=FALSE],set.train2.pca$x[,1:best.dims])

summary(set.train2.pca)

yPCA <- PCADat[,1,drop=FALSE]
xPCA <- PCADat[,-1,drop=FALSE]

fPCA <- as.formula(paste(colnames(yPCA), paste (colnames(xPCA), collapse=" + "), sep=" ~ "))  # new formula

pca.model <- lm(fPCA,PCADat)
summary(pca.model)

model <- lm(f,set.train2)
summary(model)

tested <- set.test2[,1]

predictions <- predict(model,set.test2)

predictions2 <- predict(pca.model, data.frame(predict(set.train2.pca, set.test2)))

if(normalizeResponse=="Y")
{
  predictions <- (predictions * trainParam$std[1]) + trainParam$mean[1]
  predictions2 <- (predictions2 * trainParam$std[1]) + trainParam$mean[1]
  tested <- (tested * trainParam$std[1]) + trainParam$mean[1]
}

rmse(predictions,tested)
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
diagnostic_plots_lm(pca.model)
diagnostic_plots_lm(model)

finalModel <- lm(f,data2)
summary(finalModel)

diagnostic_plots_lm(finalModel)

#using prior derived PCA's, prior best.dim's

PCAData <- cbind(predict(trainParam, data2)[,1,drop=FALSE],data.frame(predict(set.train2.pca, predict(trainParam, data2)))[,1:best.dims])

y2 <- PCAData[,1,drop=FALSE]
x2 <- PCAData[,-1,drop=FALSE]
  
f2 <- as.formula(paste(colnames(y2), paste (colnames(x2), collapse=" + "), sep=" ~ "))  # new formula

finalModelPCA <- lm(f2,PCAData)

summary(finalModelPCA)

#using newly derived PCA's, prior best.dim's (not sure if I should re-derive, but the point is to find the best hparm's on a subset)
set.final <- predict(trainParam,data2)[,unlist(predictors)]
final.pca <- prcomp(set.final[,-1])
set_final_pca <-cbind(set.final[,1,drop=FALSE],final.pca$x[,1:best.dims])

finalModelPCA <- lm(f2,set_final_pca)
summary(finalModelPCA)

library(gtools)

set.final$Groups = quantcut(set.final[,1],3)
cname <- colnames(data2[,1,drop=FALSE])
levels(set.final$Groups) <- c(paste("Low",cname),paste("Medium",cname),paste("High",cname))

g <- ggbiplot(final.pca, obs.scale = 1, var.scale = 1, 
              labels=data[rownames(set.final),1],
              groups = set.final$Groups,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')

p1 <- fviz_pca_biplot(final.pca, repel = TRUE,
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                #palette = c("#00AFBB",  "#FC4E07"),
                col.ind = "contrib",
                col.var = "contrib"
                )

ggpubr::ggarrange(p1,g)
gridExtra::grid.arrange(p1,g, ncol=2)

options(rgl.printRglwidget = TRUE)

bg3d("white")
test <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

pca3d(final.pca,group=set.final$Groups , show.scale=TRUE, show.plane = FALSE, show.labels = test ,show.centroids = TRUE,show.ellipses=FALSE, show.axe.titles = TRUE, show.group.labels=TRUE, biplot=TRUE)

rglwidget()

dataSet <- cbind(data,cooks.distance(finalModel), hatvalues(finalModel), rstudent(finalModel),finalModel$residuals,dffits(finalModel))

colnames(dataSet) <- c("State", colnames(data)[-1],"cooks.distance","leverage","studentized","standardResidual","dffitts")
summary(dataSet)
dataSet

diagnostic_plots_lm(finalModelPCA)
fviz_contrib(final.pca, choice = "var", axes = 1:best.dims, top = best.dims)

df <- data.table(dataSet[order(dataSet$Poverty),])
datatable(df, rownames = TRUE)

d <- fviz_contrib(final.pca, choice = "var", axes = 1:best.dims, top = best.dims)

d <- d$data$name[order(d$data$contrib,decreasing=TRUE)][1:3]

d <- na.omit(d)

cor.plot(PCOR(data[,c(unlist(predictors))]))

#https://www.statmethods.net/graphs/scatterplot.html

pairs.panels(data[,c(unlist(predictors))],method = "pearson", # correlation method
             pch=21,
             #hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             bg=c("green","yellow","red")[set.final$Groups]
             )


cor.plot(cor(set_final_pca))

scatterplotMatrix(data[,c(unlist(predictors))],ellipse = TRUE,lot.points = TRUE,regLine = TRUE,smooth=TRUE)
scatterplotMatrix(data[,c(unlist(predictors))],groups=set.final$Groups,by.groups = TRUE,ellipse = TRUE,lot.points = TRUE,regLine = TRUE,smooth=TRUE)

#scatterplotMatrix(set_final_pca,ellipse = TRUE,lot.points = TRUE,regLine = TRUE,smooth=TRUE)
#scatterplotMatrix(set_final_pca,groups=set.final$Groups,by.groups = TRUE,ellipse = TRUE,lot.points = TRUE,regLine = TRUE,smooth=TRUE)

set_final_pca

cbind(set.final[,-4],as.numeric(set.final$Groups))

wss <- (nrow(set.final)-1)*sum(apply(set.final[,-4],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(set.final[,-4], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(cbind(set.final[,-4],as.numeric(set.final$Groups)), 3) # 5 cluster solution
# get cluster means 
aggregate(cbind(set.final[,-4],as.numeric(set.final$Groups)),by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(cbind(set.final[,-4],as.numeric(set.final$Groups)), fit$cluster)


source("3dPlot.R")