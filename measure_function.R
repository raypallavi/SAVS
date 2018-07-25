# Function for calculating MCC, TPR, TNR
meas.fun=function(beta.true,beta.est){
  betaT.ind=beta.true
  betaT.ind[betaT.ind!=0]=1
  betaE.ind=beta.est
  betaE.ind[betaE.ind!=0]=1
  
  s.est=sum(betaE.ind==1)
  tab=table(betaT.ind,betaE.ind)
  if(s.est>0){
    t11=sqrt(tab[1,1]+tab[1,2])
    t12=sqrt(tab[1,1]+tab[2,1])
    t21=sqrt(tab[2,2]+tab[1,2])
    t22=sqrt(tab[2,2]+tab[2,1])
    
    mcc=(tab[1,1]*tab[2,2] - tab[1,2]*tab[2,1])/(t11*t12*t21*t22)
    tpr=tab[2,2]/(tab[2,2]+tab[2,1])
    tnr=tab[1,1]/(tab[1,1]+tab[1,2])
  }else{
    mcc=0
    tpr=0
    tnr=1
  }
  return(list("MCC"=mcc,"TPR"=tpr,"TNR"=tnr))
}