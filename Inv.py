import math
import scipy
import scipy.linalg 
import numpy as np
import os
import pandas as pd
import time
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter

##value of lambda, could be changed to  your own choice
lam=1e-6
##change wls to True if want use WLS method
wls=False

##Use proper funciton to load GWAS summary statistics
beta=np.loadtxt("path")
sd=np.loadtxt("path")
##Use proper function to load SNP, should be NA filled and proper scaled
snp="path"
snp=np.array(snp)
n=np.shape(snp)[0]
p=np.shape(snp)[1]

if wls==True:
  beta=np.divide(beta,sd)
  cxt=np.divide(snp.T,sd).T
  xxt=np.matmul(cxt,cxt.T)
  a1=np.diag(xxt)
  a2=a1+lam
  np.fill_diagonal(xxt,a2)
  xxtinv=np.linalg.inv(xxt)
  reg=np.matmul(xxtinv,cxt)
  yhat=(n-1)*np.matmul(reg,beta)
  
else:
  xxt=np.matmul(snp,snp.T)
  a1=np.diag(xxt)
  a2=a1+lam
  np.fill_diagonal(xxt,a2)
  xxtinv=np.linalg.inv(xxt)
  reg=np.matmul(xxtinv,snp)
  yhat=(n-1)*np.matmul(reg,beta)
  

##use proper function to save the result
np.savetxt("path",yhat)
