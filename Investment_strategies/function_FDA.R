##replace NA's with their previous values
NA_rep<-function(x, colx){                       ##x: a data frame, colx: which column of the data frame
  
  ind_miss<-which(is.na(x[, colx]))            ##where the missing data is  
  ind_miss
  for(i in ind_miss){                           ##use a "for" loop to replace the missing value with 
    ##its previous one
    x[i, colx]<-x[i-1, colx]
    
  }
  
  x
  
}

##write functions to calculate different returns
##holding period return
retx<-function(x){            ##x: price vector, should be a numeric vector
  
  x[-1]/x[-length(x)]-1  
  
}

##log return
logrx<-function(x){           ##x: price vector, should be a numeric vector     
  
  diff(log(x))
  
}
my_skewness<-function(x){
  T<-length(x)
  y<-x-mean(x)
  T*sqrt(T-1)/(T-2)*sum(y^3)/(sum(y^2)^(3/2))
}

my_kurtosis<-function(x){
  T<-length(x)
  y<-x-mean(x)
  f1<-T*(T+1)*(T-1)/((T-2)*(T-3))
  f2<-3*((T-1)^2)/((T-2)*(T-3))
  f1*sum(y^4)/(sum(y^2)^2)-f2
}

my_acf1<-function(x, na.action = na.fail){            
  acf(x, plot = F, na.action = na.action)[[1]][2]
}   

VaR_samplex<-function(x, amountx, alphax){     ##xx: return vector, amountx: money
  
  qx<-as.numeric(quantile(x, alphax))
  
  qx*amountx
  
}

ES_samplex<-function(x, amountx, alphax){   ##xx: return vector, amountx: money
  
  qx<-as.numeric(quantile(x, alphax))
  indx<-x<=qx
  
  mean(x*indx)/alphax*amountx
  
}

##constants A, B and C
ABC_mvp<-function(r){                          ##r: return data          
  
  n<-dim(r)[2]                               ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)          ##(in-sample) mean return
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )        ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  inv_covx<-solve(covx)                      ##inverse covariance matrix
  onex<-matrix(rep(1,n), n, 1)                 ##vector of ones
  
  ##constants A, B and C
  Ax<-t(mux)%*%inv_covx%*%mux                ##constant A
  Bx<-t(mux)%*%inv_covx%*%onex               ##constant B
  Cx<-t(onex)%*%inv_covx%*%onex              ##constant C
  
  list(A = Ax, B = Bx, C = Cx)
  
}

##portfolio weights of the gmvp and short the negative one
gmvp_wx<-function(r){                          ##r: return data         
  
  n<-dim(r)[2]                               ##number of assets               
  covx<-cov(r, use = "complete.obs" )        ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)                       
  inv_covx<-solve(covx)                      ##inverse covariance matrix
  onex<-matrix(rep(1,n), n, 1)                 ##vector of ones
  
  ##constant
  ABCx<-ABC_mvp(r)
  Cx<-as.numeric(ABCx$C)
  
  ##optimal weight vector
  wx<-1/Cx*inv_covx%*%onex
  
  return(wx)  
  
}
  

##portfolio weights of the mvp 
mvp_wx<-function(r, mu_targ){                ##r: return data, mu_targ: required target expected return              
  
  n<-dim(r)[2]                               ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)          ##(in-sample) mean return vector      
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )        ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)                       
  inv_covx<-solve(covx)                      ##inverse covariance matrix
  onex<-matrix(rep(1,n), n, 1)                 ##vector of ones
  
  ##constants A, B and C
  ABCx<-ABC_mvp(r)
  Ax<-as.numeric(ABCx$A)                     ##constant A
  Bx<-as.numeric(ABCx$B)                     ##constant B
  Cx<-as.numeric(ABCx$C)                     ##constant C
  
  
  ##Lagrange multipliers, delta and gamma
  deltax<-(Cx*mu_targ-Bx)/(Ax*Cx-Bx^2)       ##Lagrange multipler delta 
  deltax<-as.numeric(deltax)                 ##transform to scalar, very important
  gammax<-(Ax-Bx*mu_targ)/(Ax*Cx-Bx^2)       ##Lagrange multiplier gamma
  gammax<-as.numeric(gammax)                 ##transform to scalar, very important
  
  ##optimal weight vector
  wx<-deltax*inv_covx%*%mux + gammax*inv_covx%*%onex
  
  return(wx)  
  
}


##portfolio weights of the mvp with a risk-free asset
rf_mvp_wx<-function(r, mu_targ, rf){         ##r: return data, mu_targ: required target return                
  ##rf: risk-free return
  
  n<-dim(r)[2]                               ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)          ##(in-sample) mean return vector  
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )        ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  inv_covx<-solve(covx)                      ##inverse covariance matrix
  onex<-matrix(rep(1,n), n, 1)               ##vector of ones
  
  ##other vector and constant
  rpx<-mux-rf*onex                           ##risk-premium vector  
  Dx<-t(rpx)%*%inv_covx%*%rpx
  Dx<-as.numeric(Dx)
  
  ##optimal weight vector
  wx<-(mu_targ-rf)/Dx*inv_covx%*%rpx
  wx<-c(1-sum(wx), wx)                       ##remember to add the weight of the risk-free asset: 1-sum(wx)
  
  return(wx)  
}


##portfolio weights of the tangency portfolio
tan_wx<-function(r, rf){                     ##r: return data, rf: risk-free return              
  
  n<-dim(r)[2]                               ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)          ##mean return vector
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )        ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  inv_covx<-solve(covx)                      ##inverse covariance matrix
  onex<-matrix(rep(1,n), n, 1)               ##vector of ones
  
  ##other vector and constant
  rpx<-mux-rf*onex                           ##risk-premium vector 
  Dx<-t(onex)%*%inv_covx%*%rpx
  Dx<-as.numeric(Dx)
  
  ##optimal weight vector
  
  wx<-1/Dx*inv_covx%*%rpx
  
  return(wx)  
  
}


## mvp with required expected portfolio return
mvp_sdx<-function(r, mu_targ){              ##r: return data,rf: risk-free return
  
  n<-dim(r)[2]                              ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)         ##(in-sample) mean return vector
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )       ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  inv_covx<-solve(covx)                     ##inverse covariance matrix
  onex<-matrix(rep(1,n),n,1)                ##vector of ones
  
  ##constants A, B and C
  ABCx<-ABC_mvp(r)
  Ax<-as.numeric(ABCx$A)                     ##constant A
  Bx<-as.numeric(ABCx$B)                     ##constant B
  Cx<-as.numeric(ABCx$C)                     ##constant C
  
  sigma2x<-(Ax-2*Bx*mu_targ+Cx*(mu_targ^2))/(Ax*Cx-Bx^2)
  
  return(sqrt(sigma2x))
  
}


##mvp's standard deviation
##no risk-free asset
mvp_sdx<-function(r, mu_targ){              ##r: return data,rf: risk-free return
  
  n<-dim(r)[2]                              ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)         ##(in-sample) mean return vector
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )       ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  inv_covx<-solve(covx)                     ##inverse covariance matrix
  onex<-matrix(rep(1,n),n,1)                ##vector of ones
  
  ##constants A, B and C
  ABCx<-ABC_mvp(r)
  Ax<-as.numeric(ABCx$A)                     ##constant A
  Bx<-as.numeric(ABCx$B)                     ##constant B
  Cx<-as.numeric(ABCx$C)                     ##constant C
  
  sigma2x<-(Ax-2*Bx*mu_targ+Cx*(mu_targ^2))/(Ax*Cx-Bx^2)
  
  return(sqrt(sigma2x))
  
}

##with the risk-free asset
rf_mvp_sdx<-function(r, mu_targ, rf){       ##r: return data, mu_targ: target return, 
  ##rf: risk-free return
  
  n<-dim(r)[2]                              ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)         ##(in-sample) mean return vector
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )       ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  inv_covx<-solve(covx)                     ##inverse covariance matrix
  onex<-matrix(rep(1,n),n,1)                ##vector of ones
  
  ##other vector and constant
  rpx<-mux-rf*onex
  Ex<-t(rpx)%*%inv_covx%*%rpx
  Ex<-as.numeric(Ex)
  
  sigma2x<-((mu_targ-rf)^2)/Ex
  
  return(sqrt(sigma2x))
  
}

##tangency portfolio's mean return and stdev
tan_mr_sdx<-function(r, rf){                   ##r: return data, rf: risk-free return   
  
  n<-dim(r)[2]                                 ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)            ##(in-sample) mean return vector
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )          ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  inv_covx<-solve(covx)                        ##inverse covariance matrix
  onex<-matrix(rep(1,n),n,1)                   ##vector of ones
  
  ##other vectors and constants
  rpx<-mux-rf*onex                             ##risk premium vector    
  
  Dx<-t(onex)%*%inv_covx%*%rpx
  Dx<-as.numeric(Dx)
  
  Ex<-t(rpx)%*%inv_covx%*%rpx
  Ex<-as.numeric(Ex)
  
  Fx<-t(mux)%*%inv_covx%*%rpx
  Fx<-as.numeric(Fx)
  
  ##portfolio's expected return and varaince
  mu_tan<-Fx/Dx
  sigmax<-sqrt(((mu_tan-rf)^2)/Ex)
  
  list(mu_tan = mu_tan, sd_tan = sigmax)  
}


##gmvp using quadratic programming, need the package "quadprog"
gmvp_wx_quad<-function(r){              ##r: return data, mu_targ: required target expected return 
  
  n<-dim(r)[2]                          ##number of assets               
  covx<-cov(r, use = "complete.obs" )   ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1,n),n,1)            ##vector of ones
  
  A<-onex                               ##A matrix
  b0<-matrix(1, 1, 1)                   ##b vector
  d0<-matrix(rep(0,n), n, 1)            ##d vector
  
  solve.QP(Dmat = covx, dvec = d0, 
           Amat = A, bvec = b0, meq = 1)
  
}

##mvp by using quadratic programming, need the package "quadprog"
##no risk-free assets
mvp_wx_quad<-function(r, mu_targ){      ##r: return data, mu_targ: required target expected return 
  
  n<-dim(r)[2]                          ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)     ##(in-sample) mean return vector 
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )   ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1,n),n,1)            ##vector of ones
  
  A<-cbind(mux, onex)                   ##A matrix
  b0<-matrix(c(mu_targ, 1), 2, 1)       ##b vector
  d0<-matrix(rep(0,n), n, 1)            ##d vector
  
  solve.QP(Dmat = covx, dvec = d0, 
           Amat = A, bvec = b0, meq = 2)
  
}

##with risk-free assets
rf_mvp_wx_quad<-function(r, mu_targ, rf){ ##r: return data, mu_targ: required target expected return
  ##rf: risk-free return
  
  n<-dim(r)[2]                          ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)     ##(in-sample) mean return vector 
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )   ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1,n),n,1)            ##vector of ones
  
  A<-mux-rf*onex                        ##A matrix
  b0<-mu_targ-rf                        ##b vector
  d0<-matrix(rep(0,n),n,1)              ##d vector  
  
  solve.QP(Dmat = covx, dvec = d0, 
           Amat = A, bvec = b0, meq = 1)
  
}

##no-shortsale gmvp
nsgmvp_wx_quad<-function(r){
  
  n<-dim(r)[2]                          ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)     ##(in-sample) mean return vector
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )   ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1, n), n, 1)          ##vector of ones
  
  A<-onex                               ##A matrix
  Ix<-diag(1, n)                        ##An identity matrix for no-shortsale constraints
  A<-cbind(A, Ix)
  b0<-matrix(c(1, rep(0, n)), 1+n, 1)   ##b_0
  d0<-matrix(rep(0, n), n, 1)           ##d_0  
  
  return(solve.QP(Dmat = covx, dvec = d0, Amat = A, bvec = b0, meq = 1))
  
}  


##no-shortsale mvp
nsmvp_wx_quad<-function(r, mu_targ){
  
  n<-dim(r)[2]                          ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)     ##(in-sample) mean return vector
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )   ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1, n), n,1)          ##vector of ones
  
  A<-cbind(mux, onex)                   ##A matrix
  Ix<-diag(1, n)                        ##An identity matrix for no-shortsale constraints
  A<-cbind(A, Ix)
  b0<-matrix(c(mu_targ, 1, rep(0, n)), 2+n, 1)      ##b_0
  d0<-matrix(rep(0, n), n, 1)                       ##d_0  
  
  return(solve.QP(Dmat = covx, dvec = d0, Amat = A, bvec = b0, meq = 2))
  
}

##no shortsale mvp with the risk-free asset
rf_nsmvp_wx_quad<-function(r, mu_targ, rf){
  
  n<-dim(r)[2]                          ##number of assets               
  mux<-apply(r, 2, mean, na.rm = T)     ##(in-sample) mean return vector
  mux<-as.matrix(mux)
  covx<-cov(r, use = "complete.obs" )   ##sample covariance matrix, note the usage of "use" vs na.rm
  covx<-as.matrix(covx)  
  onex<-matrix(rep(1, n), n,1)          ##vector of ones
  
  A<-mux-rf*onex                        ##Amat
  Ix<-diag(1, n)                        ##An identity matrix for no-shortsale constraints
  A<-cbind(A, Ix)
  b0<-c(mu_targ-rf, rep(0, n))          ##b_0
  d0<-matrix(rep(0, n), n, 1)           ##d_0  
  
  return(solve.QP(Dmat = covx, dvec = d0, Amat = A, bvec = b0, meq = 1))
  
}


##function for calculating component contribution
por_ccx<-function(Sigmax, wx){                            ##sigmax: covariance matrix
  ##wx: portfolio weight
  wx<-as.numeric(wx)
  Sigmax<-as.matrix(Sigmax)
  varx<-t(wx)%*%Sigmax%*%wx
  varx<-as.numeric(varx)
  mcx<-Sigmax%*%wx/sqrt(varx)                             ##marginal contribution
  cx<-wx*mcx                                              ##contribution
  per_cx<-cx/sqrt(varx)*100                               ##percentage contribution
  
  list(marginal_contribution = mcx, 
       contribution = cx,
       percentage_contribution = per_cx)
  
}

