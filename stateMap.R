library(ggbiplot)
library(factoextra)
library(plotly)
library(reshape2)
library(dbscan)
library(data.table)
library(whitening)
library(reticulate)
library(labdsv)
library(optpart)
library(scclust)
library(reticulate)
library(qwraps2)

source("functions.R")

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

if(FALSE)
{
  #dis.bc <- dsvdis(set,'bray/curtis')
  #dis <- dist(set)
  #opt.5 <- optpart(5,dis)
  #sil.5 <- optsil(opt.5,dis,100) # make take a few minutes
  #summary(silhouette(sil.5,dis.bc))
  ## Not run: 
  #plot(silhouette(sil.5,dis.bc))
}

v_folds=sample(rep(1:numFolds, length=nrow(set)))

#unique(sc_clustering(distances(set), 10))

sets <- lapply(1:numFolds, function(k)
{#k=1
  set.seed(k)
  
  v <- set[which(v_folds!=k),,drop=FALSE]
  #nrow(set[which(v_folds!=k),,drop=FALSE])
  #print(k)
  #km <- kmeans(v, centers=size)
  const_kmeans <- constrained.kmeans(p=matrix(1,nrow(v)),a=v,std=FALSE,k=size,b=matrix(3,size),imax=1000,seed=k,details=FALSE)
  
  inner_nums <- c(1:numFolds)[1:numFolds!=k]
  
  errors <- lapply(inner_nums, function (i)
    {#i=6
    #print(i)
    #inner_nums
    
      inner_set <- inner_nums[which(inner_nums!=i)]
      #leave one MORE fold out
      #v_folds %in% inner_set
    
      t <- set[v_folds %in% inner_set,,drop=FALSE]
      
      km2 <- kmeans(t, const_kmeans$centers)
      return(list(km2$totss,km2$centers))
    })
  
  return(list(mean(unlist(lapply(errors, `[[`, 1))),const_kmeans$centers))
  
  
  #print(t)
  #fitted(km, method = c("centers", "classes"))
})

if(FALSE)
{
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
}

clusterErrors <- unlist(lapply(sets, `[[`, 1))

centersCluster <- sets[which(clusterErrors==min(clusterErrors))][[1]][[2]]

fit <- constrained.kmeans(p=matrix(1,nrow(set)),a=set,std=FALSE,k=size,b=matrix(3,size),imax=1000,seed=k,details=FALSE)

fit$tot.withinss

#abs(1-fit$betweenss/fit$tot.withinss)

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

#convert from matrix to df
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
# append cluster assignment
mydata <- data.frame(cbind(data$State,mydata, fit$cluster))
#kind of redundant
#mydata <- data.frame(cbind(data$State,set, sil.5$clustering))

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