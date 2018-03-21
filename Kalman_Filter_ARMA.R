## Kalman Filter 

# Kalman ARMA estimation 

State_Space_Representation = function(y,p,q){
  #browser()
  
  r = max(p,q+1)
  
  
  #Dummy Phi and Theta 
  phi = c(0.1,0.1)
  theta = c(0.1)
  
  A = matrix(0,r,r)
  A[1:p,1] = phi 
  A[1:(r-1),2:r] = diag(r-1)
  
  R = rep(0,r)
  R[1] = 1
  R[2:(q+1)] = theta 
  
  Z = rep(0,r)
  Z[1] = 1
  
  Q = tcrossprod(R,R)
  
  L = 0
  SS = 0
  
  
  #Initializing State and Covariance 
  Sigma_t = matrix(solve(diag(r^2) - (A %x% A),as.vector(t(Q))),r,r) 
  xhat_t = rep(0,r)
  
  logsum = 0
  sumsq  = 0
  
  for (i in 1:(length(y)-1)){
    xhat_t1t = A %*% xhat_t
    Sigma_t1t = A %*% Sigma_t %*% t(A) + Q
    yhat_t1t = t(Z) %*% xhat_t1t
    Omega = drop(t(Z) %*% Sigma_t1t %*% Z)
    delta_t1 = (Sigma_t1t %*% Z)/Omega
    innov = drop(y[i+1] - yhat_t1t)
    xhat_t1t1 = xhat_t1t + delta_t1*innov
    Sigma_t1t1 = Sigma_t1t - tcrossprod(delta_t1,Z)%*%Sigma_t1t
    
    logsum = logsum + log(Omega)
    sumsq = sumsq + (innov^2)/Omega
    
    xhat_t = xhat_t1t1
    Sigma_t = Sigma_t1t1
  }
  L = logsum + length(y)*log(sumsq)
  sigmahat = sqrt(sumsq/length(y))
  
  return(list('Loglikelihood' = L, 'Sigmahat' = sigmahat))
}


