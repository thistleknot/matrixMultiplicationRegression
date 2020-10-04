library(plotly)
library(reshape2)

#load data
test2 <- cbind(dataSet[,paste(d)],set.final$Groups,test)
#paste(d)
#test2 <- cbind(dataSet[,c("Income","Population","Unemployed")],set.final$Groups,test)
#d[1]
#data_lm <- lm(Poverty ~ Income + Population,data = test2)

#Graph Resolution (more important for more complex shapes)
graph_reso <- 1000

xname <- noquote(paste0("~",d[[1]]))

#Setup Axis
axis_x <- seq(min(test2[,paste(d[1]),drop=FALSE]), max(test2[,paste(d[1]),drop=FALSE]), by = graph_reso)
axis_y <- seq(min(test2[,paste(d[2]),drop=FALSE]), max(test2[,paste(d[2]),drop=FALSE]), by = graph_reso)
axis_z <- seq(min(test2[,paste(d[3]),drop=FALSE]), max(test2[,paste(d[3]),drop=FALSE]), by = graph_reso)

#can't figure out how to pass dynamic names to x,y,z

hcolors=c("green","blue","red")[test2$set.final]
data_plot <- plot_ly(test2[,1:3],
                     x = ~test2[,1],
                     y = ~test2[,2], 
                     z = ~test2[,3],
                     text = test2$test, # EDIT: ~ added
                     type = "scatter3d", 
                     mode = "text",
                     marker = list(color = hcolors))

data_plot

