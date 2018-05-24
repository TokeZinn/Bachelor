library(forecast)
library(ggplot2)


prewhite = function(x, y, arima_order, titl){
  if(is.ts(x) == FALSE){x = ts(x)}
  if(is.ts(y) == FALSE){y = ts(y)}

  par(mfrow = c(2,1))
  #plot(x)
  #plot(y)
  par(mfrow = c(1,1))
  
  model = arima(x, order = arima_order, method = "ML", include.mean = FALSE)
  
  pwx = model$residuals
  
  model2 = Arima(y, order = arima_order, method = "ML", model = model, include.mean = FALSE)
  pwy = model2$residuals
  
#  ccf(pwx, newpwy, na.action = na.omit)

  ggCcf(pwx, pwy, lag.max = NULL, type = c("correlation", "covariance"),
        plot = TRUE, na.action = na.contiguous) + ggtitle(titl)
  
}

#prewhite(x,y,c(1,0,1), tit = "Hej")


#### Model specifier

Model_Specifier_ekso = function(data,p_max=5,q_max=5){
  Best_AIC = Inf
  
  for(i in 0:p_max) {
    for(j in 0:q_max){
      model = arima(data , order = c(i,0,j), include.mean = FALSE, method = "ML")
      AIC_temp = model$aic
      
      if( AIC_temp < Best_AIC){
        Best_AIC = AIC_temp
        Best_pair = c(i,j)
      }
    }
  }
  return(Best_pair)
}


#### Hour to daily tranformer

HoursToDaysTransformer = function(X) {
  
  #browser()
  
  Y = X
  
  n = nrow(Y)/24
  m = ncol(Y)
  
  for(i in 1:n){
    for(j in 1:m){
      
      Y[i,j] =  sum(Y[(i:(i-1+24)),j])
      
    }
    
    Y = Y[-((i+1):(i+24-1)),]
    
  }
  
  return(Y)
}





