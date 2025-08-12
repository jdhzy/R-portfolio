#' Perform common multiple linear regression calculations
#' @export
mlr <- function(X, Y) { 
  
  ### Basic Info ###
  n   <- length(Y) # [a number] sample size
  p   <- dim(X)[2] - 1 # [a number] number of predictors
  
  ### Common Multiple Regression Metrics ###
  bhat    <- solve(t(X)%*%X) %*% t(X) %*% Y # [vector: (p+1)*1] beta hat
  Yhat    <- X%*%bhat # [vector:n*1] Yhat
  res  <- Y - Yhat # [vector: n*1] residuals 
  
  ### Squares stuff ### 
  sse     <- sum(res^2) 
  sst     <- var(Y) * (n-1)
  ssm     <- sst - sse
  
  mse     <- sse / (n-p-1)
  msm     <- ssm / p
  mst     <- sst / (n-1)
  
  rmse    <- sqrt(sse/n) # root of mse
  
  sigsq   <- mse # [a number] variance(Y|X)
  sig     <- sqrt(mse) # [a number] standard deviation
  sighat  <- mse*solve(t(X)%*%X) # [matrix: (p+1)*(p+1)] covariance
  sebhat <- sqrt(diag(sighat)) # [(p+1) numbers] standard error k~(1,p+1)
  sbhat   <- bhat / sebhat # [vector: (p+1)*1] standardized coefficient value
  
  R2      <- 1 - sse/sst # coefficient of determination
  R2adj   <- 1 - mse/mst # adjusted coefficient of determination
  
  Fstat   <- msm/mse
  vmsm    <- p
  vmse    <- n-p-1
  pval    <- pf(Fstat, p, n-p-1, lower.tail = F) # p-value
  
  results <- list('n'       = n,
                  'p'       = p,
                  
                  'bhat'    = bhat, 
                  'Yhat'    = Yhat, 
                  'res'     = res, 
                  
                  'ssm'     = ssm, 
                  'msm'     = msm,
                  'sst'     = sst,
                  'mst'     = mst,
                  'sse'     = sse, 
                  'mse'     = mse, 
                  'rmse'    = rmse,
                  
                  'sigsq'   = sigsq,
                  'sig'     = sig,
                  'sighat'  = sighat,
                  'sebhat'  = sebhat,
                  
                  'R2'      = R2, 
                  'R2adj'   = R2adj,
                  
                  'Fstat'   = Fstat,
                  'vmsm'    = vmsm, 
                  'vmse'    = vmse,
                  'pval'    = pval)
  return(results)
}