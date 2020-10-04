#library("car")
#library("rgl")

#data(iris)

#sep.l <- iris$Sepal.Length
#sep.w <- iris$Sepal.Width
#pet.l <- iris$Petal.Length

#scatter3d(x = sep.l, y = pet.l, z = sep.w, surface=FALSE, labels = rownames(iris), id.n=nrow(iris))

test2 <- cbind(dataSet[,paste(d)],set.final$Groups,test)
x <- test2[,1]
y <- test2[,2]
z <- test2[,3]

# 3D plot with the regression plane

scatter3d(x = x, y = y, z = z, groups = test2$`set.final$Groups`,
          grid = FALSE, fit = "linear",ellipsoid = TRUE, surface=TRUE,
          surface.col = c("green", "blue", "red"),
          #showLabels(x = x, y = y, z = z, labels=test2$test, method="identify",n = nrow(test2), cex=1, col=carPalette()[1], location=c("lr"))
          #labels = test2$test,
          #id=list(method="identify"),
          #id.n=test2$test
          )



rglwidget()