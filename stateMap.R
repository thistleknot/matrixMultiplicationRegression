library(ggbiplot)
library(factoextra)
library(plotly)
library(reshape2)
library(dbscan)
library(data.table)
library(whitening)
library(reticulate)
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

seed <- sample(1:100,1)
print(seed)
set.seed(seed)
PCA=FALSE
whiten=TRUE

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
  
  
  if(PCA)
  {
    #set <- whiten(as.matrix(set))
    set <- whiten(as.matrix(set),method=c("ZCA"),center=TRUE)
    set.pca <- prcomp(set, center=FALSE, scale=FALSE)
  }
  #decorrelate (i.e. whiten)

  if(whiten)
  {
    set <- data2
    set <- whiten(as.matrix(set),method=c("ZCA"),center=TRUE)
  }
  colnames(set) <- colnames(data2)
  
}

best.dims = ncol(data2)

if(PCA)
{
  get_eigenvalue(set.pca)
  
  wss <- (nrow(set.pca$x)-1)*sum(apply(set.pca$x,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(set.pca$x,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  size <- which(wss==tail(wss[wss>mean(wss)],1))
  #size <- 5
  fit <- kmeans(set.pca$x, size) # 5 cluster solution

}

if(whiten)
{
  wss <- (nrow(set)-1)*sum(apply(set,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(set,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  size <- which(wss==tail(wss[wss>mean(wss)],1))
  #size <- 5
  fit <- kmeans(set, size) # 5 cluster solution
} 

scores <- mclapply(1:100, function(i)
{
  set.seed(i)
  
  km <- kmeans(set, centers=size)
  print(i)
  
  ratio <- km$betweenss/km$tot.withinss
  return(abs(ratio-1))
})

set.seed(which(scores==min(unlist(scores))))

fit <- kmeans(set, centers=size)

abs(1-fit$betweenss/fit$tot.withinss)

if(FALSE)
{
  d <- dist(set, diag = T, method = "euclidean")
  fit <- hclust(d, method="ward") 
  plot(fit) # display dendogram
  groups <- cutree(fit, k=size) # cut tree into 5 clusters
  # draw dendogram with red borders around the 5 clusters 
  rect.hclust(fit, k=size, border="red")
  #View(fit)
}

#kNNdistplot(rbind(set.train,set.test)[,colnames(set.train2)], k=knn_model$k.opt)

#knn_model <- knn.reg.bestK(set.pca$x)

#eps <- median(kNNdist(rbind(set.train,set.test)[,colnames(set.train2)], k=knn_model$k.opt))

#cl <- dbscan(set.pca$x, eps = tail(which(wss>=mean(wss)),1), tail(which(wss>=mean(wss)),1))
#pairs(set.pca$x, col = cl$cluster+1L)

# get cluster means 
if(PCA)
{
  aggregate(cbind(set.pca$x),by=list(fit$cluster),FUN=mean)
}
if(whiten)
{
  aggregate(cbind(set),by=list(fit$cluster),FUN=mean)
}
# append cluster assignment
mydata <- do.call(cbind,lapply(1:ncol(set),function(x)
{
  if(PCA)
  {
    return(as.numeric(set.pca$x[,x]))
  }
  if(whiten)
  {
    return(as.numeric(set[,x]))
  }
   
}))

if(PCA)
{
  colnames(mydata) <- colnames(set.pca$x)
  }
if(whiten)
{
  {colnames(mydata) <- colnames(set)}
}
#pairs(mydata[,1:9],col=fit$cluster)

mydata <- data.frame(cbind(data$State,mydata, fit$cluster))

if(PCA) colnames(mydata) <- c("state",colnames(set.pca$x),"Cluster")
if(whiten) colnames(mydata) <- c("state",colnames(set),"Cluster")

#fviz_contrib(set.pca, choice = "var", axes = 1:3)
#fviz_contrib(set.pca, choice = "var", axes = 1:ncol(set.pca$x))

cbind(aggregate(. ~ mydata$Cluster, data = data2, mean),mydata %>% group_by(Cluster) %>% summarise(no_rows = length(Cluster)))

cbind(aggregate(. ~ mydata$Cluster, data = data2, sd),mydata %>% group_by(Cluster) %>% summarise(no_rows = length(Cluster)))

orange <- "#C9592E"

thanksgiving <- gtrends("thanksgiving",geo = "US", time = "now 1-d")

thanksgivingStates <- thanksgiving$interest_by_region
thanksgivingStates$fips <-fips(thanksgivingStates$location)

colnames(thanksgivingStates) <- c("state","hits","keyword","geo","gprop")

plot_usmap(data = mydata, values = "Cluster",  color = orange, labels=FALSE) + 
  scale_colour_brewer(palette = "Greens")

#best.dims

#colnames(data2)

bg3d("white")
states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

pca3d(set.pca, group = mydata$Cluster , show.scale=TRUE, show.plane = FALSE, show.labels = states ,show.centroids = TRUE,show.ellipses=FALSE, show.axe.titles = TRUE, show.group.labels=TRUE, biplot=TRUE)

rglwidget()

#aggregate(data2,by=list(fit$cluster),FUN=mean)

hcolors=c("green","blue","red","black","orange")[as.factor(mydata$Cluster)]
hsymbols=c('circle', 'square', 'diamond','circle-open', 'square-open', 'diamond-open', 'cross', 'x')

#hcolors=c("green","blue","red","black")[test2$`set.final$Groups`]
data_plot <- plot_ly(mydata[,2:4],
                     y = ~mydata[,2],
                     x = ~mydata[,3],
                     z = ~mydata[,4],
                     text = states, #mydata$state, # EDIT: ~ added
                     type = "scatter3d", 
                     mode = "text",
                     marker = list(color = hcolors
                                   #, symbol=hsymbols
                                   ))

data_plot