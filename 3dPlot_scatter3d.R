#library("car")
#library("rgl")

#data(iris)

#sep.l <- iris$Sepal.Length
#sep.w <- iris$Sepal.Width
#pet.l <- iris$Petal.Length

#scatter3d(x = sep.l, y = pet.l, z = sep.w, surface=FALSE, labels = rownames(iris), id.n=nrow(iris))

test2 <- cbind(dataSet[,paste(d)],set.final$Groups,test)
X <- dataSet[,paste(d[1])]
Y <- dataSet[,"Poverty"]
Z <- dataSet[,paste(d[2])]

# 3D plot with the regression plane

scatter3d(x = X, y = Y, z = Z, groups = test2$`set.final$Groups`,
          grid = FALSE, fit = "linear",ellipsoid = TRUE, surface=TRUE,
          surface.col = c("green", "blue", "red"),
          id=list(method = "mahal", n = length(test2$test), labels = test2$test)
        
)

  
rglwidget()