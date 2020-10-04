
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
          
          rmse <- rmse(predV,unlist(actualV))
          
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

diagnostic_plots <- function(model, data)
{#model
  #dev.off()
  layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))
  plot(model$fitted.values,model$residuals)
  lines(loess.smooth(model$fitted.values,model$residuals), col="red", lty=1, lwd=2)
  text(model$fitted.values[order((abs(model$residuals)), decreasing = TRUE)[1:3]],model$residuals[order((abs(model$residuals)), decreasing = TRUE)[1:3]], data[,1][order((abs(model$residuals)), decreasing = TRUE)[1:3]], cex=1, pos=1, col="red")
  abline(h=0,lty=2)
  
  #plot(model,2)
  plot((qqnorm(rstandard(model),,plot.it = FALSE)$x),rstandard(model))
  qqline(rstandard(model))
  text((qqnorm(rstandard(model),,plot.it = FALSE)$x)[order(abs(rstandard(model)), decreasing = TRUE)[1:3]], rstandard(model)[order(abs(rstandard(model)), decreasing = TRUE)[1:3]], data[,1][order(abs(rstandard(model)), decreasing = TRUE)[1:3]], cex=1, pos=c(1,3,4), col="red")
  
  scatter.smooth(fitted(model), sqrt(abs(rstandard(model))), ylim=c(0, 1.6))
  text(fitted(model)[order((abs(model$residuals)), decreasing = TRUE)[1:3]],sqrt(abs(rstandard(model)))[order((abs(model$residuals)), decreasing = TRUE)[1:3]], data[,1][order((abs(model$residuals)), decreasing = TRUE)[1:3]], cex=1, pos=1, col="red")
  
  #https://www.statmethods.net/advgraphs/axes.html
  
  leverageFlagLower=2*(ncol(model$model))/nrow(model$model)    
  leverageFlagHigher=3*(ncol(model$model))/nrow(model$model)    
  cookFlag=.5
  
  n <- nrow(model$model)
  k <- length(model$coefficients)-1
  cv <- 2*sqrt(k/n)
  
  labels <- c()
  for (cd in 1:length(hatvalues(model)))
  {
    if(hatvalues(model)[cd] >= leverageFlagLower || abs(rstudent(model)[cd]) >= 3)
    {
      #print("true")
      labels[cd] <- data[,1][cd]
    }
    if(!(hatvalues(model)[cd] >= leverageFlagLower || abs(rstudent(model)[cd]) >= 3))
    {
      #print("false")
      labels[cd] <- NA
    }
    
  }
  
  scatter.smooth(hatvalues(model),studres(model),xlim=c(0, 1),ylim=c(-3.1, 3.1))
  text(hatvalues(model), studres(model), labels, cex=1, pos=1, col="red")
  abline(v = c(leverageFlagLower,leverageFlagHigher), col=c("blue", "red"))
  abline(h = c(3,-3))
  
  labels <- c()
  for (cd in 1:length(cooks.distance(model)))
  {
    if(cooks.distance(model)[cd] >= cookFlag || abs(rstudent(model)[cd]) >= 3)
    {
      #print("true")
      labels[cd] <- data[,1][cd]
    }
    if(!(cooks.distance(model)[cd] >= cookFlag || abs(rstudent(model)[cd]) >= 3))
    {
      #print("false")
      labels[cd] <- NA
    }
    
  }
  
  plot(cooks.distance(model),rstudent(model),xlim=c(0, 1),ylim=c(-3.1, 3.1))
  text(cooks.distance(model),rstudent(model), labels, cex=1, pos=1, col="red")
  abline(v = cookFlag,h = c(3,-3))
  
  labels <- c()
  for (cd in 1:length(dffits(model)))
  {#cd=1
    if(abs(dffits(model)[cd]) >= cv || abs(rstudent(model)[cd]) >= 3)
    {
      #print("true")
      labels[cd] <- data[,1][cd]
    }
    if(!(abs(dffits(model)[cd]) >= cv || abs(rstudent(model)[cd]) >= 3))
    {
      #print("false")
      labels[cd] <- NA
    }
    
  }
  
  plot(dffits(model),studres(model),ylim=c(-3.1, 3.1))
  text(dffits(model), studres(model), labels, cex=1, pos=1, col="red")
  abline(v = c(cv,-cv), h=c(3,-3), lty = c(2,2,1,1))    
  
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  hist(model$residuals)
  hist(rstudent(model))
  plot(model,5)
  plot(model,6)
  
  layout(matrix(1:length(names), 2, 2, byrow = TRUE))
  eff.pres <- allEffects(model)
  plot(eff.pres)
  
  leveragePlots(model)
  
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

#need to look into
#https://rpubs.com/milesdwilliams15/328471
peplot <- function(mod,var,ci=.95, plot_points = "n",
                   xlab=var,ylab=names(mod[12]$model)[1],
                   main="Partial Effect Plot",
                   pe_lty=1,pe_lwd=3,pe_col="black",
                   ci_lty=1,ci_lwd=1,ci_col="black",
                   pch_col="black",pch_ty=19
                   ){
  modDat <- mod[12]$model
  modDat1 <- modDat[,-1]
  modDat2 <- modDat[,which(names(modDat)!=var)]
  x <- resid(lm(modDat1[,var] ~., data=modDat1[,which(names(modDat1)!=var)]))
  y <- resid(lm(modDat2[,1] ~ ., modDat2[,-1]))
  part <- lm(y~x)
  wx <- par("usr")[1:2]
  new.x <- seq(wx[1],wx[2],len=100)
  pred <- predict(part, new=data.frame(x=new.x), interval="conf",
                  level = ci)
  ylim=c(min(pred[,"lwr"]),max(pred[,"upr"]))
  plot(x,y,type=plot_points,xlab=xlab,ylab=ylab,
       ylim=ylim,col=pch_col,pch=pch_ty,
       main=main)
  lines(new.x,pred[,"fit"],lwd=pe_lwd,lty=pe_lty,col=pe_col)
  lines(new.x,pred[,"lwr"],lwd=ci_lwd,lty=ci_lty,col=ci_col)
  lines(new.x,pred[,"upr"],lwd=ci_lwd,lty=ci_lty,col=ci_col)
}

#https://towardsdatascience.com/keeping-an-eye-on-confounds-a-walk-through-for-calculating-a-partial-correlation-matrix-2ac6b831c5b6

PCOR <- function(x, type = c("raw", "cor")) {
  
  type <- match.arg(type)
  
  if (type == "raw") {
    x <- scale(x)
    R <- (t(x) %*% x) / (nrow(x) - 1)
  } else  {
    R <- x
  }
  
  ind <- unique(dim(R))
  R_inv <- ginv(R)
  ZM <- matrix(rep(0, len = (ind*ind)), nrow = ind)
  diag(ZM) <- diag(R_inv)
  D <- ginv(ZM)
  AICOV <- D %*% R_inv %*% D
  diag(ZM) <- diag(AICOV)
  D  <- ginv(sqrt(ZM))
  AICOR <- D %*% AICOV %*% D
  pcor <- AICOR
  pcor[upper.tri(pcor)] <- -pcor[upper.tri(pcor)]
  pcor[lower.tri(pcor)] <- -pcor[lower.tri(pcor)]
  dimnames(pcor) <- list(colnames(R), colnames(R))
  return(pcor)
  
}  
