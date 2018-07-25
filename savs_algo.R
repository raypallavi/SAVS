# Function to implement SAVS algorithm:
savs.fun=function(X,beta.in){
  # Input: X is nXp design matrix; beta.in: posterior mean of beta (pX1)
  # Output: Sparse estimate beta.est
  beta.est=double()
  mu=(beta.in)^(-2)
  for(j in 1:p){
    xtx = t(X[,j])%*%X[,j]
    if(mu[j] >= abs(beta.in[j])*xtx){
      beta.est[j]=0
    }else{
      beta.est[j]=sign(beta.in[j])*(abs(beta.in[j])*xtx - mu[j])/xtx
    }
  }
  return(beta.est)
}

