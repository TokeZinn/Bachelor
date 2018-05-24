## State Space and Kalman Filter


## Simulation of State Space ## ---- 
State_Space_Simulation = function(model,x_0, sim_num, seed){
  #browser()
  library('MASS')
  set.seed(seed)
  A = as.matrix(model$A)
  C = as.matrix(model$C)
  Q = as.matrix(model$Q)
  H = as.matrix(model$H)
  
  if (missing(x_0)){
    x_0 = rep(0,length(A[1,]))
  }
  
 
  
  X = list()
  Y = list()
  
  X[[1]] = x_0
  
  for (i in 1:sim_num){
    X[[i+1]] = A %*% X[[i]] + MASS::mvrnorm(n = 1, rep(0, length(as.matrix(Q)[1,])), as.matrix(Q))
    Y[[i]] = C %*% X[[i]] + MASS::mvrnorm(n = 1, rep(0,length(as.matrix(H)[1,])), as.matrix(H))
  }
  X[[sim_num+1]] = NULL
  
  return(list('X' = X, 'Y' = Y))
  
}
Kalman_Filter = function(model, x_0, sig_0, Y){
  #browser()
  
  #Define matrices
  A = as.matrix(model$A)
  C = as.matrix(model$C)
  Q = as.matrix(model$Q)
  H = as.matrix(model$H)
  
  if (missing(x_0)){
    x_0 = rep(0,length(A[1,]))
  }
  if (missing(sig_0)){
    sig_0 = diag(length(A[1,]))
  }
  
  
  #Find dimensions
  dim_Y = dim(Y[[1]])[1]
  dim_X = length(A[1,])
  
  X_est = list()
  X_pred = list()
  P_est = list()
  P_pred = list()
  Y_pred = list()
  eps = list()
  Sigmas = list()
  K = list()
  Sig_est = list()
  Y_est = list()
  
  i = 1 
  X_est[[i]] = x_0
  P_est[[i]] = sig_0
  
  for (y in Y){
    # Predict X_t+1 given t
    X_pred[[i]] = A %*% X_est[[i]] 
    
    #Predict Variance of X_t+1 given t
    P_pred[[i]] = A %*% P_est[[i]] %*% t(A) + Q
    
    #Predict Y
    Y_pred[[i]] = C %*% X_pred[[i]]
    
    #Calculate error
    eps[[i]] = y - Y_pred[[i]]
    
    #Calculate Prediction Variance of Y
    Sigmas[[i]] = C %*% P_pred[[i]] %*% t(C) + H
    
    #Calculate the Kalman Gain
    K[[i]] = P_pred[[i]] %*% t(C) %*% solve(Sigmas[[i]])
    
    #Make correction / Estimates of X_t+1 given t+1 
    X_est[[i+1]] = X_pred[[i]] + K[[i]] %*% eps[[i]]
    
    #Make correction / Estimates of variance of X_t+1 given t+1 
    P_est[[i+1]] = (diag(dim_X) - K[[i]]%*% C) %*% P_pred[[i]] 
    
    #Other posterior estimates.
    Sig_est[[i]] = C %*% P_est[[i+1]] %*% t(C) + H
    Y_est[[i]] = C %*% X_est[[i+1]] 
    
    i = i+1 
  }
  
  return(list('X_est' = X_est, 'P_est' = P_est, 'X_pred' = X_pred, 'P_pred' = P_pred,'Y_est' = Y_est, 'Y_Sig_est' = Sig_est, 'Y_pred' = Y_pred,'eps' = eps, 'Sigma' = Sigmas, 'KalGain'=K))
}
Kalman_Predictor = function(model, result, steps){
  #browser()
  A = model$A
  C = model$C
  Q = model$Q
  H = model$H
  
  X_est = result$X_est[[length(result$X_est)]]
  P_est = result$P_est[[length(result$P_est)]]
  
  X_pred = list()
  P_pred = list()
  Y_pred = list()
  Sig_pred = list()
  
  X_pred[[1]] = A %*% X_est
  P_pred[[1]] = A %*% P_est %*% t(A) + Q
  
  for (i in 1:steps){
    Y_pred[[i]] = C %*% X_pred[[i]]
    Sig_pred[[i]] = C %*% P_pred[[i]] %*% t(C) + H
    if (i == steps){
      break
    }
    X_pred[[i+1]] = A %*% X_pred[[i]]
    P_pred[[i+1]] = A %*% P_pred[[i]] %*% t(A) + Q
  }
  return(list('X_pred' = X_pred,'Y_pred' = Y_pred,'P_pred' = P_pred,'Sig_pred' = Sig_pred))
}

## Setup Test Model ## ----
iter = 1000
model_test = list(A = diag(2),#cbind(c(0.8,0.3),c(0.2,0.6)), 
                  C = t(c(0.2,0.1)), #t(c(1,0)), 
                  Q = 0.5*diag(2),
                  H = 0.3)

SSS = State_Space_Simulation(model = model_test, sim_num = iter, seed = 321)

Kalman_SSS = Kalman_Filter(model = model_test, Y = SSS$Y)
## Plot of State Space (2 dimensions) ## ----

plot(unlist(SSS$Y),type="l")


ggplot() + 
  geom_line(aes(x = 1:length(unlist(SSS$Y)), y = unlist(SSS$Y), colour = "Y process")) + 
  xlab("Index") + 
  ylab("Y") + 
  scale_color_manual(name = "Series", values = c("Y process" = "#000000"))

plot(unlist(SSS$X)[1:length(unlist(SSS$X)) %% 2 == 0],type ="l")
plot(unlist(SSS$X)[1:length(unlist(SSS$X)) %% 2 != 0],type ="l")

plot(unlist(SSS$Y), type = "l")
lines(unlist(Kalman_SSS$Y_pred), col ="red")
lines(unlist(Kalman_SSS$Y_est), col = "blue")


X_1 = unlist(SSS$X)[1:length(unlist(SSS$X)) %% 2 != 0]
X_2 = unlist(SSS$X)[1:length(unlist(SSS$X)) %% 2 == 0]
Index = 1:iter


Unlist_Kalman_SSS = unlist(Kalman_SSS$X_est)
x_est = Unlist_Kalman_SSS[1:2002%%2==0]
y_est = Unlist_Kalman_SSS[1:2002%%2!=0]

plotly_data_kalman = data.frame(X_1 = x_est, X_2 = y_est, Index = 1:1001)
plotly_data = data.frame(X_1,X_2,Index)

## State Space Plots ---- 

library(plotly)

p <- plot_ly(plotly_data, x = ~X_1, y = ~X_2, z = ~Index, type = 'scatter3d', mode = 'lines',
             opacity = 1, line = list(width = 3, color = "black"), name = "X process") %>%
  add_trace(data = plotly_data_kalman, x = ~X_1, y = ~X_2, z = ~Index, opacity = 1, name = "X estimates", line = list(color = "red", width = 3)) %>%
  layout(showlegend = TRUE,
  legend = list(orientation = 'h', x = 0.4))

api_create(p, filename = "test-randomwalk")
p #Just plot

## Prediction 
it = 100

prediction = Kalman_Predictor(model_test, Kalman_SSS,steps = it)

Y = c(unlist(SSS$Y))
Upper = c(rep(0,length(unlist(SSS$Y))),qnorm(0.975, unlist(prediction$Y_pred), sqrt(unlist(prediction$Sig_pred))))
Lower = c(rep(0,length(unlist(SSS$Y))),qnorm(0.025, unlist(prediction$Y_pred), sqrt(unlist(prediction$Sig_pred))))
X = 1:(length(unlist(SSS$Y)))

Upper_est = c(qnorm(0.975, unlist(Kalman_SSS$Y_est),sqrt(unlist(Kalman_SSS$Y_Sig_est))))
Lower_est = c(qnorm(0.025, unlist(Kalman_SSS$Y_est),sqrt(unlist(Kalman_SSS$Y_Sig_est))))

Upper_pred = c(qnorm(0.975, unlist(Kalman_SSS$Y_pred),sqrt(unlist(Kalman_SSS$Sigma))),rep(0,it))
Lower_pred = c(qnorm(0.025, unlist(Kalman_SSS$Y_pred),sqrt(unlist(Kalman_SSS$Sigma))),rep(0,it))

data_plot = data.frame("Index" = X,"Values" = Y)
prediction_plot = data.frame("Index" = (length(unlist(SSS$Y))+1):(length(unlist(SSS$Y))+length(unlist(prediction$Y_pred))), "Values" = unlist(prediction$Y_pred), "Upper" = Upper, "Lower" = Lower)
estimation_plot = data.frame("Index" = X, "Values" = unlist(Kalman_SSS$Y_est), "Upper" = Upper_est, "Lower" = Lower_est)

cols = c("Data" = "#000000", "Estimation" = "#ff4500", "Prediction"= "#69d500")
cols_fill = c("Confidence" = "#ff4500", "Prediction"= "#7cfc00")

size = c("Data" = 0.7, "Estimation" = 0.7, "Prediction" = 1)

ggplot() + 
  geom_line(data = data_plot, aes(x= Index , y = Values, colour = "Data", size = "Data")) + 
  geom_line(data = prediction_plot, aes(x = Index, y=Values, colour = "Prediction", size = "Prediction")) + 
  geom_ribbon(data = prediction_plot, aes(x = Index, ymin = Lower, ymax = Upper, fill = "Prediction"), alpha = 0.5) +
  geom_line(data = estimation_plot, aes(x = Index, y=Values, colour = "Estimation", size = "Estimation")) + 
  geom_ribbon(data = estimation_plot, aes(x = Index, ymin = Lower, ymax = Upper, fill = "Confidence"), alpha = 0.5)+
  scale_color_manual(name = "Lines", values = cols) + 
  scale_fill_manual(name = "Interval", values = cols_fill) + 
  scale_size_manual(values = size, guide = "none")
  
