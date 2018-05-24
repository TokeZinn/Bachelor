#Bootstrap Method of DF statistic

library(ggplot2)
library(tseries)
library(stats)


T = 400
B = 99999
set.seed(711)


boot_func = function(y, T, B){
  # Residuals, residual mean, and re-centered residuals
  e = rep(0,T)
  for(i in 2:(length(e))){e[i]=y[i]-y[i-1]}
  e_hatbar = 1/(T-1) * sum(e)
  e_recen = e - e_hatbar
  
  # Bootstrapping
  M = matrix(y[1],T,B)
  j = 1
  while(j<(B+1)){
    resampled_e = sample(e_recen, replace = TRUE)
    for(i in 2:T){M[i,j]=M[i-1,j]+resampled_e[i]}
    j = j +1}
  
  return(M)
}

G_T = function(y, T){
  rho = 0; rho2 = 0
  for(i in 2:T){
    rho = rho + y[i]*y[i-1]
    rho2 = rho2 + y[i-1]^2
  }
  rho = rho/rho2
  G_T = (T-1)*(rho-1)
  return(G_T)
  #cat("Rho is", rho, "and G_T is", G_T, sep = " ") 
}

stat = function(M, y, T, B){
  # G_T for each bootstrap
  G_vec = rep(0,B)
  for(j in 1:B){
    G_vec[j] = G_T(M[,j],T)
  }
  # Vector of rep G_T of original
  vec = rep(G_T(y,T),length(G_vec))
  
  # p-value
  p = sum(G_vec < vec) / (B + 1)
  cat("The p-value is", p, sep = " ")
  #return(p)
}

stat2 = function(M, T, B){
  # G_T for each bootstrap
  G_vec = rep(0,B)
  for(j in 1:B){
    G_vec[j] = G_T(M[,j],T)
  }
  return(G_vec)
}


boot_y = boot_func(y, T, B)


Statistic_g = stat2(boot_y,T,B)
quantile(Statistic_g,probs = c(0.01,0.025,0.05,0.95,0.975,0.99))
#hist(Statistic_g)


ggplot(data.frame(Statistic_g),aes(Statistic_g)) +
  geom_histogram(binwidth = 0.5,color="black",fill="white") +
  xlab("Dickey-Fuller statistic") + ylab("Frequency") +
  ggtitle("Histogram of the DF-statistic")





