gendata10=function(p,set=1,method,s_sign,s=10,n=200,sigmaT=1.5,seed=555)
{ ## Input:
  ## n: Sample size
  ## p: Dimension
  ## s: Number of non-zero coefficients
  ## s_sign: sign of the non-zero coefficients, sX1 vector of 0 or 1
  ## sigmaT: True error standard deviation
  ## set: Takes value 1; 
  ##      If set=1, non-zero values of beta = {0.75, 1, 1.25,1.5, 1.75, 2, 2.25, 2.5,2.75,3}
  ##      Else The non-zero coefficients are generated according to Fan and Lv (2007)
  ## method: Takes values {1, 2, 3, 4, 5};
  ##         If method=1, rows of X are independently generated from Np(0,Ip)
  ##         If method=2, rows of X are independently generated from Np(0,\Sigma)
  ##                      where diagonals of \Sigma are 1 and all off-diagonals are 0.5
  ##         If method=3, rows of X are independently generated from Np(0,\Sigma)
  ##                      where diagonals of \Sigma are 1 and all off-diagonals are 0.5^|j-j'|
  ##                      i.e. TOEPLITZ STRUCTURE
  ##         If method=4, rows of X are independently generated from Np(0,\Sigma)
  ##                      where diagonals of \Sigma are 1 and all off-diagonals are 0.7^|j-j'|
  ##                      i.e. TOEPLITZ STRUCTURE
  ##         If method=5, rows of X are independently generated from Np(0,\Sigma)
  ##                      where diagonals of \Sigma are 1 and all off-diagonals are 0.9^|j-j'|
  ##                      i.e. TOEPLITZ STRUCTURE
  ## seed: random seed 
  
  ## Output: 
  ## y: Response, a n by 1 vector
  ## X: Design matrix of dimension n by p
  ## betaT: True coefficient vector with s non-zero elements and (p-s) zeros
  
  set.seed(seed)
  library(MASS)
  
  if(method==1)
  {
    X=matrix(rnorm(n*p),n,p)
  }
  else if(method==2)
  {
    X=matrix(mvrnorm(n,rep(0,p),diag(0.5,p)+matrix(0.5,p,p)),n,p)
  }
  else if(method==3)
  {
    X=matrix(mvrnorm(n,rep(0,p),toeplitz(0.5^(0:(p-1)))),n,p)
  }
  else if(method==4)
  {
    X=matrix(mvrnorm(n,rep(0,p),toeplitz(0.7^(0:(p-1)))),n,p)
  }
  else if(method==5)
  {
    X=matrix(mvrnorm(n,rep(0,p),toeplitz(0.9^(0:(p-1)))),n,p)
  }
  else
  {
    stop("Invalid value for method")
  }
  
  # 
  if(set==1)
  {
    beta.true=(-1)^s_sign*(c(0.75, 1, 1.25,1.5, 1.75, 2, 2.25, 2.5,2.75,3))
  }
  else
  {
    a=4*log(n)/sqrt(n) # Refer to Fan and Lv (2007)
    beta.true=(-1)^s_sign*(a+abs(rnorm(s)))
  }
  
  betaT=c(beta.true,rep(0,p-s))
  y=X%*%betaT+rnorm(n)*sigmaT
  result=list("Response"=y,"Design_matrix"=X,"True_Beta"=betaT)
  return(result)
}
# End of function