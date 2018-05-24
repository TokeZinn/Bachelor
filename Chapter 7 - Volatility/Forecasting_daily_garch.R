

rm(list=ls()) #Clear all
source("Expanded_sorting.R")

library(rugarch)

DK1 = ts(Daily2017$DK1 - mean(Daily2017$DK1), frequency = 7)
STL_DK1 = stl(DK1 , s.window = "periodic")
season = STL_DK1$time.series[,1]
Deseasoned = ts(DK1 - STL_DK1$time.series[,1], frequency = 1)
Daily18 = ts(Daily2018$DK1 - mean(Daily2017$DK1),start = 366,frequency = 1)
Daily18 = Daily18 - season[2:(length(Daily18)+1)]
Data = ts(c(Deseasoned,Daily18))
Testset = ts ( c( Deseasoned[365] , Daily18) , start = 365  ) 

## Forecasting with Armagarch

spec = ugarchspec(mean.model = list(armaOrder = c(2,3),include.mean = FALSE),
                  variance.model = list(model = 'sGARCH', 
                                        garchOrder = c(2,2)), distribution = "norm")
Arma_garch = ugarchfit(spec,Data, solver = "hybrid",out.sample = 81)

fit = Arma_garch@fit$fitted.values


#Function for 1 step predictions and intervals
OneStep = function( X, Y , AR = 5 , MA = 4, arch=2, garch=0) {
  #browser()
  All_Data = c( X, Y)
  n = length(X)
  m = length(Y)
  OneStepPredictions = rep(0,m)
  OneUpper = rep(0,m)
  OneLower = rep(0,m)
  OneSigma2 = rep(0,m)
  
  
  for (i in 1:m) {
    Current_Data = All_Data[-((n+i):(n+m))]
    spec = ugarchspec(mean.model = list(armaOrder = c(AR,MA),include.mean = FALSE),
                      variance.model = list(model = 'sGARCH',
                                            garchOrder = c(arch,garch)), 
                      distribution = "norm")
    object = ugarchfit(spec,All_Data, solver = "hybrid",out.sample = m-i+1)
    full_fit = object@fit
    coef = full_fit$coef ; res = full_fit$residuals
    arma_fit = Arima(Current_Data,order = c(AR,0,MA),include.mean = F,
                     fixed = c(coef[1:(AR+MA)]))
    
    len = length(Current_Data)
    forecast = ugarchforecast(object,n.ahead = 1,
                              n.roll = 0)
    
    fitted = full_fit$fitted.values
    hatsigma = forecast@forecast$sigmaFor
    OneStepPredictions[i] = forecast(arma_fit, h = 1)$mean 
    
    MAX = max(AR,MA)
    xt = c()
    
    if (AR >= 1){
      X_AR = cbind(Current_Data)
      xt = c(xt,Current_Data[len])
      if (AR >=2){
        for (k in 2:AR){
          X_AR = cbind(X_AR,lag(X_AR[,k-1]))
          xt = c(xt,Current_Data[len-k+1])
        }
      }
    }
    
    
    if (MA>= 1){
      X_MA = cbind(res)
      xt = c(xt, res[len])
      if (MA>=2){
        for (j in 2:MA){
          X_MA = cbind(X_MA, lag(X_MA[,j-1]))
          xt = c(xt,res[len-j+1])
        }
      }
    }
    
    if(AR>=1 & MA>=1){
      X = cbind(X_AR[MAX:(len-1),], X_MA[MAX:(len-1),])
    }else if(AR>=1){
      X = X_AR[MAX:(len-1),]
    }else if(MA>=1){
      X = X_MA[MAX:(len-1),]
    }
    
    X = cbind(X) 
    xt = c(xt)

    se = hatsigma*sqrt(1+(t(xt)%*%(solve(t(X)%*%X))%*%xt))
    OneUpper[i] = qnorm(0.975)*se+
      OneStepPredictions[i]
    OneLower[i] = OneStepPredictions[i] + 
      qnorm(0.025)*se
    OneSigma2[i] = hatsigma^2
    
    
    
  }
  Output = data.frame("Predictions" = OneStepPredictions, "Upper" = OneUpper,
                      "Lower" = OneLower, "PredVariance" = OneSigma2)
  return(Output)
  
}


pred = OneStep( X = Deseasoned, Y = Daily18,AR = 2, MA = 3, arch = 2, garch = 2)


# write.table(Deseasoned, "C:/Users/Thomas/Dropbox/P6/R-dokumenter/Kode til Kap 8/Deseasoned.txt", 
#                         sep="\t")

# Export predictions.
# write.table(pred, "C:/Users/Thomas/Dropbox/P6/R-dokumenter/Kode til Kap 7/Predictions.txt",
#             sep="\t")

pred = read.csv("Predictions.txt",sep = "\t")

RMSE = sqrt(mean((pred$Predictions-Daily18)^2)); RMSE

sum(pred$Upper > Daily18 & pred$Lower < Daily18)/length(pred$Lower)


# Defining plots
{

p1 = ggplot() + 
  geom_line(aes( x = c(Daily2017$X__1[300:365],Daily2018$X__1) ,
                 y = ts( c(Deseasoned[300:364], Testset ) , start = 1 ) , 
                 colour = "Observed values") ) +
  geom_line(aes( x = c(Daily2017$X__1[365],Daily2018$X__1) ,
                 y = ts ( c(Deseasoned[365], pred$Predictions) ,
                          start = 365 ) , colour = "Predictions" ) ) +
  geom_ribbon(aes(x = Daily2018$X__1
                  , ymin = pred$Lower , ymax = pred$Upper) ,
              alpha = 0.2 , fill = "blue") +
  theme(legend.title = element_blank() ) + xlab("Time") +
  ylab("Deseasoned and demeaned prices") +
  scale_color_manual(values = c("black", "red"))

p2 = ggplot() + 
  geom_line(aes( x = c(Daily2018$X__1) ,
                 y = ts( c(Testset[2:length(Testset)] ) , start = 1 ) , 
                 colour = "Observed values") ) +
  geom_line(aes( x = c(Daily2018$X__1) ,
                 y = ts ( c(pred$Predictions) ,
                          start = 365 ) , colour = "Predictions" ) ) +
  geom_ribbon(aes(x = Daily2018$X__1
                  , ymin = pred$Lower , ymax = pred$Upper) ,
              alpha = 0.2 , fill = "blue") +
  theme(legend.title = element_blank()) + xlab(NULL) +
  ylab("Prices") +
  scale_color_manual(values = c("black", "red"))

testmod = ugarchfit(spec,Data, solver = "hybrid")
length(testmod@fit$sigma)

p4 = ggplot() + 
  geom_line(aes( x = c(Daily2017$X__1[300:365],Daily2018$X__1) ,
                 y = ts( c(testmod@fit$sigma[300:446]^2) , start = 1 ) , 
                 colour = "Estimated Variance") ) +
  geom_line(aes( x = c(Daily2018$X__1[-1]) ,
                 y = ts( c(predsig$Predictions[-1] ) , start = 1 ) , 
                 colour = "Predicted Variance") ) + 
  theme(legend.title = element_blank() ) + xlab("Time") +
  ylab("Variance") + scale_y_continuous(limits = c(20,80)) +
  scale_color_manual(values = c("black", "red"))
}

source("Multiplot.R")
multiplot(p2,p3)
multiplot(p1,p4)

MAE_OneStep = mean( abs(Daily18 - pred$Predictions ) )
arima(Deseasoned,order = c(2,0,3),include.mean = F,  fixed = c(Arma_garch@fit$coef[1:5]))

ggplot() + 
  geom_line(aes( x = c(Daily2017$X__1[300:365],Daily2018$X__1) ,
                 y = ts( c(Deseasoned[300:364], Testset ) , start = 1 ) , 
                 colour = "Observed values") ) +
  geom_line(aes( x = c(Daily2017$X__1[365],Daily2018$X__1) ,
                 y = ts ( c(Deseasoned[365], temp$Predictions) ,
                          start = 365 ) , colour = "Predictions" ) ) +
  geom_ribbon(aes(x = Daily2018$X__1
                  , ymin = temp$Lower , ymax = temp$Upper) ,
              alpha = 0.2 , fill = "blue") +
  #geom_line(aes(x = Daily2018$X__1, y = predsig$Predictions)) +
  theme(legend.title = element_blank() ) + xlab("Time") +
  ylab("Deseasoned and demeaned prices") +
  scale_color_manual(values = c("black", "red"))





