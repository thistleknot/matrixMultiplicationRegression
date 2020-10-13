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

#Doing PCA on ZCA data results in same distances!

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
  
  #trainParam <- caret::preProcess(as.matrix(data2),method=c("center", "scale"))
  #set <- predict(trainParam, data2)
  
  #set <- whiten(as.matrix(set))
  set <- data2
  set <- whiten(as.matrix(set),method=c("ZCA"),center=TRUE)
  colnames(set) <- colnames(data2)
  set.pca <- prcomp(set, center=FALSE, scale=FALSE)
  set <- set.pca$x
  
  #decorrelate (i.e. whiten)

  colnames(set) <- colnames(data2)
  
}

best.dims = ncol(data2)

#silhouette
if(TRUE)
{
  k <- 4:15
  
  avg_sil <- sapply(k, function(x)
  {
    silhouette_score(x,set)
  })
  plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
  
  size=which(avg_sil==max(avg_sil))+(min(k)-1)
}

#size=size/2

#find best center via highest bss/wss ratio using cluster size of silhouette
if(TRUE)
{

  sets <- lapply(1:numFolds, function(k)
  {#k=1

    seed = k+1
    
    r <- NULL
    attempt <- 1
    while( is.null(r) && attempt <= 100 ) {
      attempt <- attempt + 1
      seed = seed+1
      set.seed(seed)
      try(
        
        r <- constrained.kmeans(p=matrix(1,nrow(set)),a=set,std=FALSE,k=size,b=matrix(3,size),imax=1000,seed=seed,details=FALSE)
      )
      
    } 
    print(attempt)
    #print(seed)
    set.seed(seed)
    const_kmeans <- constrained.kmeans(p=matrix(1,nrow(set)),a=set,std=FALSE,k=size,b=matrix(3,size),imax=1000,seed=seed,details=FALSE)
    
    #inner_nums <- c(1:numFolds)[1:numFolds!=k]
    inner_nums <- 1:numFolds
    
    errors <- lapply(inner_nums, function (i)
    {#i=1

      seed = (k+1)^2
      
      r <- NULL
      attempt <- 1
      while( is.null(r) && attempt <= 100 ) {
        attempt <- attempt + 1
        seed = seed+1
        set.seed(seed)
        try(
          #r <- constrained.kmeans(p=matrix(1,nrow(set)),a=set,std=FALSE,k=size,b=matrix(3,size),imax=1000,seed=seed,details=FALSE)
          r <- kmeans(set, const_kmeans$centers)
        )
        
      } 
      print(attempt)
      #print(seed)
      set.seed(seed)
      km2 <- kmeans(set, const_kmeans$centers)
      #km2 <- constrained.kmeans(p=matrix(1,nrow(set)),a=set,std=FALSE,k=size,b=matrix(3,size),imax=1000,seed=seed,details=FALSE)
      
      t <- cbind(set,km2$cluster)
      
      tss <- sum(abs(t)^2)
      
      wss <- lapply(1:size, function(x)
      {#x=1
        iset <- t[which(t[,11]==x),-11,drop=FALSE]
        if(nrow(iset)==1)
        {sum((iset-km2$centers[x,])^2)}
        if(nrow(iset)>1)
        {sum((rowSums((iset-km2$centers[x,])^2)))}
        
      })
      
      bss <- tss - sum(unlist(wss))
      
      score <- bss/sum(unlist(wss))
      
      return(list(score,km2$centers))
    })
    
    return(list(mean(unlist(lapply(errors, `[[`, 1))),const_kmeans$centers,const_kmeans$cluster))
    
  })
  
  clusterScores <- unlist(lapply(sets, `[[`, 1))
  
  centersCluster <- sets[which(clusterScores==max(clusterScores))][[1]][[2]]
  
  clusters <- sets[which(clusterScores==max(clusterScores))][[1]][[3]]
  
  plot(clusterScores)
  
}

max(clusterScores)

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

#convert from matrix to df
mydata <- do.call(cbind,lapply(1:ncol(set),function(x)
{
     return(as.numeric(set[,x]))
}))

colnames(mydata) <- colnames(set)

# append cluster assignment
mydata <- data.frame(cbind(data$State,mydata, clusters))

colnames(mydata) <- c("state",colnames(set),"Cluster")
states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

hcolors=rainbow(16, start=0, end=1)[as.factor(mydata$Cluster)]
hsymbols=c('circle', 'square', 'diamond','circle-open', 'square-open', 'diamond-open', 'cross', 'x')

aggMeans <- cbind(aggregate(. ~ mydata$Cluster, data = data2, mean),mydata %>% group_by(Cluster) %>% summarise(no_rows = length(Cluster)))
aggSD <- cbind(aggregate(. ~ mydata$Cluster, data = data2, SD),mydata %>% group_by(Cluster) %>% summarise(no_rows = length(Cluster)))

aggMeans[order(aggMeans$Poverty),]
aggSD[order(aggMeans$Poverty),]

plot_usmap(data = mydata, values = "Cluster",  color = hcolors[1], labels=TRUE)

fviz_contrib(set.pca, choice = "var", axes = 1:3)
fviz_contrib(set.pca, choice = "var", axes = 1:ncol(set))

bg3d("white")

pca3d(set.pca, group = mydata$Cluster , show.scale=TRUE, show.plane = FALSE, show.labels = states ,show.centroids = TRUE,show.ellipses=FALSE, show.axe.titles = TRUE, show.group.labels=TRUE, biplot=TRUE)

rglwidget()

data_plot <- plot_ly(mydata[,2:4],
                     y = ~mydata[,2],
                     x = ~mydata[,3],
                     z = ~mydata[,4],
                     text = states, #mydata$state, # EDIT: ~ added
                     type = "scatter3d", 
                     mode = "text",
                     marker = list(color = hcolors
                                   #,symbol=hsymbols
                     ))

data_plot

