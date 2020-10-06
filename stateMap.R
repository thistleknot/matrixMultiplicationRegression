library(ggbiplot)
library(factoextra)
library(plotly)
library(reshape2)
library(dbscan)

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

normalizeResponse <- "Y"

data <- read.csv(file="states.csv",header = TRUE)
rownames(data) <- data[,1]
data2 <- data[,-1]
rownames(data2) <- data[,1]

nr <- nrow(data2)

#normalization (not being used)
if(normalizeResponse=="Y")
{
  #trainParam <- caret::preProcess(as.matrix(set.train),method=c("BoxCox", "center", "scale"))
  trainParam <- caret::preProcess(as.matrix(data2),method=c("center", "scale"))
  
  set <- predict(trainParam, data2)
  
}

y <- set[,1,drop=FALSE]
x <- set[,-1,drop=FALSE]

f <- as.formula(paste(colnames(y), paste (colnames(x), collapse=" + "), sep=" ~ "))  # new formula

pls.options(parallel = makeCluster(4, type = "FORK"))

set.pcr <- pcr(f, ncol(set)-1, data = set, validation = "CV",segments = 10, segment.type = c("consecutive"))

# Find the number of dimensions with lowest cross validation error
cv = RMSEP(set.pcr)
best.dims = which.min(cv$val[estimate = "adjCV", , ]) - 1
colnames(data2)
# PCA, best.dims
#pls.model = plsr(f, data = set.train2, ncomp = best.dims)

set.pca <- prcomp(set[,-1,drop=FALSE])

#set.pca$sdev/sum(set.pca$sdev)

get_eigenvalue(set.pca)

wss <- (nrow(set.pca$x)-1)*sum(apply(set.pca$x,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(set.pca$x,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(set.pca$x, tail(which(wss>=mean(wss)),1)) # 5 cluster solution

#kNNdistplot(rbind(set.train,set.test)[,colnames(set.train2)], k=knn_model$k.opt)

#knn_model <- knn.reg.bestK(set.pca$x)

#eps <- median(kNNdist(rbind(set.train,set.test)[,colnames(set.train2)], k=knn_model$k.opt))

#cl <- dbscan(set.pca$x, eps = tail(which(wss>=mean(wss)),1), tail(which(wss>=mean(wss)),1))
#pairs(set.pca$x, col = cl$cluster+1L)

# get cluster means 
aggregate(cbind(set.pca$x),by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- do.call(cbind,lapply(1:ncol(set.pca$x),function(x)
   {
   as.numeric(set.pca$x[,x])
 }))

colnames(mydata) <- colnames(set.pca$x)
#pairs(mydata[,1:9],col=fit$cluster)

mydata <- data.frame(cbind(data$X,mydata, fit$cluster))

colnames(mydata) <- c("state",colnames(set.pca$x),"Cluster")

packages = c("gtrendsR","tidyverse","usmap")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

orange <- "#C9592E"

thanksgiving <- gtrends("thanksgiving",geo = "US", time = "now 1-d")

thanksgivingStates <- thanksgiving$interest_by_region
thanksgivingStates$fips <-fips(thanksgivingStates$location)

colnames(thanksgivingStates) <- c("state","hits","keyword","geo","gprop")

plot_usmap(data = mydata, values = "Cluster",  color = orange, labels=FALSE) + 
  scale_colour_brewer(palette = "Greens")

#load data
#test2 <- cbind(dataSet[,paste(d)],cl$cluster,test,set.final$Groups)
test2 <- mydata

fviz_contrib(set.pca, choice = "var", axes = 1:3, top = best.dims)
fviz_contrib(set.pca, choice = "var", axes = 1:7, top = best.dims)

bg3d("white")
test <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

pca3d(set.pca, group = mydata$Cluster , show.scale=TRUE, show.plane = FALSE, show.labels = test ,show.centroids = TRUE,show.ellipses=TRUE, show.axe.titles = TRUE, show.group.labels=TRUE, biplot=TRUE)

rglwidget()
#paste(d)
#test2 <- cbind(dataSet[,c("Income","Population","Unemployed")],set.final$Groups,test)
#d[1]
#data_lm <- lm(Poverty ~ Income + Population,data = test2)

#Graph Resolution (more important for more complex shapes)
graph_reso <- 10

#can't figure out how to pass dynamic names to x,y,z

hcolors=c("green","blue","red","black","orange")[as.factor(mydata$Cluster)]
hsymbols=c('circle', 'square', 'diamond','circle-open', 'square-open', 'diamond-open', 'cross', 'x')[quantcut(as.numeric(test2[,5]),3)]

#hcolors=c("green","blue","red","black")[test2$`set.final$Groups`]
data_plot <- plot_ly(test2[,2:4],
                     y = ~test2[,2],
                     x = ~test2[,3],
                     z = ~test2[,4],
                     text = test, #mydata$state, # EDIT: ~ added
                     type = "scatter3d", 
                     mode = "text",
                     marker = list(color = hcolors
                                   #, symbol=hsymbols
                                   ))

data_plot