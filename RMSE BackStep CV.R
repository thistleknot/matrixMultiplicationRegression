numFolds=10

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

library(car)
library(MASS)
library(data.table)

rmse_backstep <- function(combined)
{#combined=set.train
  
  v_folds=sample(rep(1:numFolds, length=nrow(as.data.frame(combined))))
  
  y=colnames(combined[1])
  
  RemoveColumnFlag = "searching"
  removedSet <- c()
  
  #used to hold formula's
  formulaHolder <- c()
  columnHolder <- c()
  
  while(RemoveColumnFlag!="")
  {
    print(RemoveColumnFlag)
    if (RemoveColumnFlag == "searching")
    {
      removedSet = c()
      reducedSet <- combined
      
    }
    if (RemoveColumnFlag != "searching")
    {
      removedSet = c(removedSet,RemoveColumnFlag)
      #could just iteratively remove one at a time vs this removedSet... but having a list of what I remove is useful
      reducedSet <- dplyr::select(combined,-all_of(c(removedSet)))
      
      #View(formulaHolder)
      formulaHolder <- rbind(formulaHolder,t(c(y,ncol(reducedSet)-1,bestrmse,paste(RemoveColumnFlag,collapse=","),
                                               paste(colnames(reducedSet),collapse=","))))
      columnHolder <- c(columnHolder,list(colnames(reducedSet)))
      
      colnames(formulaHolder) <- c("y","numCol","rmse","removed columns","formula")
      
      #reducedSet <- dplyr::select(combined,-all_of(removedSet))
    }
    
    cnames <- colnames(reducedSet)
    colScores=matrix(NA,ncol(reducedSet))
    
    #can't parallelize
    
    colScores <- matrix(unlist(mclapply(1:ncol(reducedSet), function(nameCol)
    {#nameCol=1
      
      #parse dataSet
      firstColumnY = reducedSet[,1,drop=FALSE]
      nonY <- reducedSet[-nameCol]
      nonY <- nonY[,-1]
      
      #full
      if(nameCol==1)
      {
        reducedSet2 <- reducedSet
      }
      
      if(nameCol!=1)
      {
        reducedSet2 <- cbind(firstColumnY,nonY)
      }
      
      if(ncol(reducedSet2)==1)
      {
        RemoveColumnFlag=""
        #didn't remove anything
        bestrmse= min(na.omit(colScores[1:length(colScores)]))
        formulaHolder <- rbind(formulaHolder,c(y,ncol(reducedSet)-1,bestrmse,paste(RemoveColumnFlag,collapse=","),paste(colnames(reducedSet[,-1]),collapse=",")))
        rmse.cv = bestrmse
        #break
      }
      #print("cv")
      if(!ncol(reducedSet2)==1)
      {
        #colScores is mclapply, doubling up on that extrapolates the threads too much and breaks the process.  Example 100 columns = 100 threads * 5, is 500 threads!
        #I've had it freeze on 24 columns...  But it makes most sense to have this be not multithreaded (outer loop vs inner loop, or at the very least, only have 1 multithreaded)
        errors <- mclapply(1:numFolds, function(k)
        {#k=1
          t <- reducedSet2[which(v_folds!=k),,drop=FALSE]
          #print(t)
          v <- reducedSet2[which(v_folds==k),,drop=FALSE]
          #print(v)
          
          model <- glm(t,family="gaussian")
          #print(model)
          
          predV <- c()
          predV <- predict(model, v[-1])
          
          predT <- c()
          predT <- model$fitted.values
          
          actualT <- c()
          actualT <- t[1]
          
          actualV <- c()
          actualV <- v[1]
          
          optimal <- InformationValue::optimalCutoff(actualT, predT, optimiseFor = "misclasserror", returnDiagnostics = FALSE)
        
          #both
          #rmse <- MAPE(predV,unlist(actualV))
          rmse <- RMSE(predV,unlist(actualV))
          
          return(rmse)
          #},mc.cores = 2)
        })
        
        rmse.cv=mean(unlist(errors))
        
        rmse.cv
        
      }
      #print("endcv")
      rmse.cv
      
    })))
    #start at 2, because 1 is full model, 2 is when we start removing factors
    
    # if above condition: if(ncol(reducedSet2)==1) was NOT triggered
    if(!RemoveColumnFlag=="")
    {
      
      bestrmse = min(colScores[2:length(colScores)])
      
      #print(paste("Removed: ",RemoveColumnFlag))
      print(ncol(reducedSet)-1)
      print(bestrmse)
      
      #don't remove y  
      if(ncol(reducedSet)<=2)
        #if (nameCol == 1)
      {
        #RemoveColumnFlag =="searching"
        RemoveColumnFlag = ""
        #formulaHolder <- rbind(formulaHolder,c(y,ncol(reducedSet)-1,bestrmse,paste(RemoveColumnFlag,collapse=","),paste(colnames(dplyr::select(reducedSet,-all_of(c(RemoveColumnFlag)))),collapse=",")))
      }
      #if (nameCol != 1)
      if(ncol(reducedSet)>2)
      {
        #I remove min, because it's that set that had min score and the set is named after the removed column (i.e. cnames reference), add 1 due to starting at 2
        #second set is 2:, first set is 1:up, so it matches 2 to first
        RemoveColumnFlag = cnames[1:length(cnames)][(which(colScores[2:length(colScores)] == bestrmse))+1]
      }
      if(length(RemoveColumnFlag)>=ncol(reducedSet)-1)
      {
        RemoveColumnFlag = ""
      }
      print(paste("Removing:",RemoveColumnFlag))
     
    }
    
  }
  
  factors <- formulaHolder[,2]
  factormax <- formulaHolder[,2][1]

  above0 <- data.frame(formulaHolder[as.double(formulaHolder[,2])>0,,drop=FALSE])
  minrmsescore <- data.frame(t(formulaHolder[as.double(above0[,3])==min(as.double(above0[,3])),-4]))

  best_columnSet <- (columnHolder[which(as.double(formulaHolder[,3])==min(as.double(formulaHolder[,3])))])

  plot(factors,formulaHolder[,"rmse"], main=y, xlab="#Factors", ylab="rmse",las=1, col="steelblue", xlim=c(as.double(factormax), 0),pch=20)
  
  #if a list of 2 (Vs a length of 4) return last element in list
  if(nrow(minrmsescore)!=1)
  {
    minrmsescore <- data.frame(t(minrmsescore[length(minrmsescore)]))
    best_columnSet <- data.frame(t(best_columnSet[length(best_columnSet)]))
    View(best_columnSet)
  }
  
  return(list(minrmsescore,best_columnSet))
}

back_step_partial_correlation <- function(innerdata)
{#innerdata=set.train
  stop= 0
  
  n=nrow(innerdata)
  threshold_t <- qt(.05, n-2, lower.tail = TRUE, log.p = FALSE)
  
  while(stop==0)
  {
    
    if(length(colnames(innerdata))<=3)
    {
      break
    }
    
    internal_Scores <- mclapply(2:length(colnames(innerdata)), function(p)
    {#p=2
      print(p)
      
      folds=sample(rep(1:numFolds, length=nrow(as.data.frame(innerdata))))

      pcors <- lapply(1:numFolds, function (k)
      {#k=2
        
        i_data <- innerdata[which(folds!=k),,drop=FALSE]
        
        y=i_data[,1,drop=FALSE]
        xsfiltered <- i_data[c(-1,-p)]
        control <- i_data[p]
        
        f1 <- as.formula(
          paste(colnames(y), 
                paste(sprintf("`%s`", colnames(xsfiltered))
                      , collapse = " + "), 
                sep = " ~ "))
        #print(f1)
        m1 <- lm(f1, data = cbind(y,xsfiltered))
        
        f2 <- as.formula(
          paste(colnames(control), 
                paste(sprintf("`%s`", colnames(xsfiltered))
                      , collapse = " + "), 
                sep = " ~ "))
        #print(f2)
        m2 <- lm(f2, data = cbind(control,xsfiltered))
        
        abs(cor(data.frame(m1$residuals),data.frame(m2$residuals)))
      })
      
      pcors <- mean(unlist(pcors))
      return(pcors)
      
    })
    
    #I'm removing min correlation between x and y controlling for rest of data
    #I want the relationship to be significant when comparing with y.
    remove_scor <- min(unlist(mclapply(internal_Scores, `[[`, 1)))
    
    t = remove_scor * sqrt(n-2)/ sqrt(1-remove_scor^2)
    
    sig <- dt(t, n-2, log = FALSE)
    
    print(sig)
    
    removeColumn <- which(
      (
        unlist(mclapply(internal_Scores, `[[`, 1))
        #add 1 because data has 1st element as y
      )==remove_scor)+1
    print(removeColumn)
    
    if(!is.na(sig))
    {
      if(sig<=.05)
      {
        stop=1
        break
      }
    }
    
    print(removeColumn)
    innerdata <- innerdata[-removeColumn]
    print(colnames(innerdata))
    
  }
  
  return(innerdata)
}

back_step_vif <- function(data)
{#data=set.train
  
  #remove aliased
  #https://stackoverflow.com/questions/28885160/vifs-returning-aliased-coefficients-in-r
  model <- lm(data)
  ld.vars <- attributes(alias(model)$Complete)$dimnames[[1]]
  nonAliasedData <- data[!colnames(data) %in% ld.vars]
  
  newData <- nonAliasedData
  
  folds=sample(rep(1:numFolds, length=nrow(as.data.frame(newData))))
  
  maxVif = 10
  
  while(maxVif >= 10)
  {
    
    vifs <- lapply(1:numFolds, function (k)
    {#k=2
    
      i_data <- newData[which(folds!=k),,drop=FALSE]
      model <- lm(i_data)
      
      #print(ld.vars)
      
      fit.new <- lm(newData)
      
      vifs <- vif(fit.new)
      
      return(vifs)
      
    })
  
    vifs <- colSums(do.call(rbind,vifs))/numFolds
    
    maxVif <- max(vifs)
    while(is.na(maxVif))
    {
      #sloppy non recursive way
      removeColumns <- colnames(newData)[findCorrelation(cor(newData))]
      newData <- dplyr::select(newData, -all_of(removeColumns))
      fit.new <- glm(newData,family="binomial",control = list(maxit = ncol(newData)))
      
      vifs <- vif(fit.new)
      maxVif <- max(vifs)
      
    }
    print(maxVif)
    if(maxVif>=10)
    {
      remove <- names(which(vifs==maxVif))
      print(remove)
    
      newData <- dplyr::select(newData,-all_of(remove))
    }
      
  }
  
  #print(colnames(newData))
  print(vifs)
  
  return(dplyr::select(data,all_of(colnames(newData))))
  
}

normalizeResponse <- "Y"

data <- read.csv(file="states.csv",header = TRUE)
data2 <- data[,-1]

nr <- nrow(data2)

seed <- sample(1:100,1)
print(seed)
set.seed(seed)

trainSetIndex <- (sample(1:(nr),(nr)*.8))
testSetIndex <- c(1:nr)[(1:nr) %in% c(trainSetIndex)==FALSE]

set.train <- data2[trainSetIndex, ]
set.test <- data2[testSetIndex,]

#normalization (not being used)
if(normalizeResponse=="Y")
{
  trainParam <- caret::preProcess(as.matrix(set.train))
  
  set.train <- predict(trainParam, set.train)
  
  set.test <- predict(trainParam, set.test)
}

set.train <- back_step_vif(set.train)

set.train <- back_step_partial_correlation(set.train)

result <- rmse_backstep(set.train)

predictors <- read.csv(text=paste (result[[2]][[1]], collapse=","),header = FALSE)

set.train <- set.train[,c(unlist(predictors))]

y <- set.train[,1,drop=FALSE]
x <- set.train[,-1,drop=FALSE]

f <- as.formula(paste(colnames(y), paste (colnames(x), collapse=" + "), sep=" ~ "))  # new formula

model <- lm(f,set.train)
summary(model)

fitted <- model$fitted.values
trained <- set.train[,1] 
tested <- set.test[,1]

if(normalizeResponse=="Y")
{
  fitted <- (fitted * trainParam$std[1]) + trainParam$mean[1]
  trained <- (trained * trainParam$std[1]) + trainParam$mean[1]
}

plot(fitted,trained)
abline(lm(fitted~trained))

cor(trained,model$fitted.values)

predictions <- predict(model,set.test)

if(normalizeResponse=="Y")
{
  predictions <- (predictions * trainParam$std[1]) + trainParam$mean[1]
  tested <- (tested * trainParam$std[1]) + trainParam$mean[1]
}

RMSE(predictions,tested)
MAPE(tested,predictions)

plot(tested,predictions)
abline(lm(tested~predictions))
cor(tested,predictions)

finalModel <- lm(f,data2)
summary(finalModel)

hist(finalModel$residuals)

par(mfrow = c(2, 3))

plot(model,1)
plot(model,2)
plot(model,3)
plot(model,4)
#plot(model,5)
plot(hatvalues(finalModel),studres(finalModel))
#plot(cooks.distance(finalModel),studres(finalModel))
plot(model,6)

leveragePlots(finalModel)
