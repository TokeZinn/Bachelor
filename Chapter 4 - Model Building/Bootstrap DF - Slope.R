#Bootstrap Method of DF statistic with intercept and slope

library(ggplot2)
library(tseries)
library(stats)

T = 400
B = 999999
set.seed(711)
y = rep(0,T)
for(i in 2:(length(y))){ y[i] = y[i-1] + rnorm(1,0,1)}

y2 = rep(0,T)
for(i in 2:(length(y2))){ y2[i] = 2 + 0.8 * y2[i-1] + rnorm(1,0,1)}

df = data.frame(y, y2)

ggplot(df, aes(seq(1,T),y)) + geom_line() + xlab("Time") + ylab("Value")
ggplot(df, aes(seq(1,T),y2)) + geom_line() + xlab("Time") + ylab("Value")

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

G_T_Slope = function(y, T){
  #browser()
  
  y1 = y[-c(1,2)]
  
  y2 = y[-c(1,T)]
  
  ones = rep(1, length(y1))
  
  time = seq(1,length(y1))
  
  X = cbind(ones,y2,time)
  
  beta = solve( crossprod(X,X) ) %*% t(X) %*%  y1
  
  G_T = (T-1)*(beta[2]-1)
  
  return(G_T)
}

#Udregning af p-værdi
stat = function(M, y, T, B){
  # G_T for each bootstrap
  G_vec = rep(0,B)
  for(j in 1:B){
    G_vec[j] = G_T_Slope(M[,j],T)
  }
  # Vector of rep G_T of original
  vec = rep(G_T_Slope(y,T),length(G_vec))
  
  # p-value
  p = sum(G_vec < vec) / (B + 1)
  cat("The p-value is", p, sep = " ")
}

stat2 = function(M, T, B){
  # G_T for each bootstrap
  G_vec = rep(0,B)
  for(j in 1:B){
    G_vec[j] = G_T_Slope(M[,j],T)
  }
  return(G_vec)
}

boot_y = boot_func(y,T,B)

Statistic_g = stat2(boot_y,T,B)
quantile(Statistic_g,probs = c(0.01,0.025,0.05,0.95,0.975,0.99))


ggplot(data.frame(Statistic_g),aes(Statistic_g)) +
  geom_histogram(binwidth = 0.5,color="black",fill="white") +
  xlab("Dickey-Fuller statistic") + ylab("Frequency") +
  ggtitle("Histogram of the DF-statistic")



