pcor_matrix <- function(data)
{
  partialCors <- list()
  
  ColNames <- colnames(data)
  
  for (i in 1:length(ColNames)) {
    
    y <- ColNames[i]
    
    covariatesAll <- ColNames[!(ColNames %in% y)]
    
    crntPcor <- double()
    
    for (j in 1:length(covariatesAll)) {
      
      covarLeftOut <- covariatesAll[j]
      
      covariatesCrnt <- covariatesAll[!(covariatesAll %in% covarLeftOut)]
      
      rhs <- paste(covariatesCrnt, collapse = " + ")
      
      lhs <- paste(y, "~")
      
      frmla <- as.formula(paste(lhs, rhs))
      
      mod1 <- lm(frmla, data)
      
      R1 <- resid(mod1)
      
      lhs <- paste(covarLeftOut, "~")
      
      frmla <- as.formula(paste(lhs, rhs))
      
      mod2 <- lm(frmla, data)
      
      R2 <- resid(mod2)
      
      crntPcor[j] <- cor(R1,R2)
      
    }  
    
    partialCors[[i]] <- append(crntPcor, 1 , i-1)
    
  }
  
  partialCorMat <- matrix(unlist(partialCors), 
                          ncol = length(ColNames), 
                          nrow = length(ColNames), 
                          dimnames = list(ColNames, ColNames ))
  
  partialCorMat  
}

