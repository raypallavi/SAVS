##With s=10
#source codes:
source("gendata10.R")
source("savs_algo.R")
source("measure_function.R")

library(ncvreg)
library(parcor)
library(horseshoe)

p=5000
n=200
s=10
method=5
sigma=1.5
mcmc=5000
brn=1000
ssign=rbinom(s,1,0.4)  #for s_sign of gendata10

dat1=gendata10(p,set=1,method,s_sign=ssign,s=s,n=n,sigmaT=sigma)
X=dat1$Design_matrix
y=dat1$Response
beta.tr=dat1$True_Beta

hs=horseshoe(y,X,method.tau="truncatedCauchy",method.sigma="Jeffreys",burn=brn,nmc=mcmc)
  
beta.in=hs$BetaHat
est.savs=savs.fun(X,beta.in)
(val.savs=meas.fun(beta.tr,est.savs))

cv_adlasso=adalasso(X,y,intercept = FALSE)
est.adlasso=cv_adlasso$coefficients.adalasso
(val.adlasso=meas.fun(beta.tr,est.adlasso))

cv_scad=cv.ncvreg(X,y,"gaussian","SCAD")
fit=cv_scad$fit
est.scad=fit$beta[,cv_scad$min][-1]
(val.scad=meas.fun(beta.tr,est.scad))

cv_mcp=cv.ncvreg(X,y,"gaussian","MCP")
fit2=cv_mcp$fit
est.mcp=fit2$beta[,cv_mcp$min][-1]
(val.mcp=meas.fun(beta.tr,est.mcp))
  
