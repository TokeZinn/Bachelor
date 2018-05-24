


rm(list=ls()) #Clear all
source("Expanded_sorting.R")

library(rugarch)
library(tseries)
library(forecast)

#Deseasoning and partitioning data
DK1 = ts(Daily2017$DK1 - mean(Daily2017$DK1), frequency = 7)
STL_DK1 = stl(DK1 , s.window = "periodic")
season = STL_DK1$time.series[,1]
Deseasoned = ts(DK1 - STL_DK1$time.series[,1], frequency = 1)
Daily18 = ts(Daily2018$DK1 - mean(Daily2017$DK1),start = 366,frequency = 1)
Daily18 = Daily18 - season[2:(length(Daily18)+1)]
Data = ts(c(Deseasoned,Daily18))
Testset = ts ( c( Deseasoned[365] , Daily18) , start = 365  ) 


#Specifying and fitting the model
source("fixed_ARMAGARCH_Specifier.R")
Arma_garch = fixed_Garch_Specifier(Deseasoned,arma_order = c(3,2), debug = F,
                            garch_order = c(2,2),Mean = FALSE,arma = T)
spec = ugarchspec(mean.model = list(armaOrder = c(3,2),include.mean = FALSE),
                  variance.model = list(model = 'sGARCH',
                                        garchOrder = c(1,2)), distribution = "norm")
Arma_garch = ugarchfit(spec,Deseasoned, solver = "hybrid")

infocriteria(Arma_garch)
Arma_garch@fit$coef
fit = Arma_garch@fit$fitted.values

ggplot() + geom_line(aes(x = Daily2017$X__1, Deseasoned,color = "Observed values")) +
  geom_line(aes(x = Daily2017$X__1,fit,color = "Fitted values")) +
  xlab("Time") + ylab("Deseasoned and demeaned prices") + 
  scale_color_manual(values = c("blue", "red")) + theme(legend.title = element_blank() )


#Model diagnostics
res = Arma_garch@fit$residuals/Arma_garch@fit$sigma
kurt = mean((res-mean(res))^4)/(mean((res-mean(res))^2))^2
skewness = mean((res-mean(res))^3)/(mean((res-mean(res))^2))^(3/2)

checkresiduals(res)

jarque.bera.test(res)

lb=c()
for (i in 9:20){
  lb = c(lb, Box.test(res,lag = i,type = c("Ljung-Box"), fitdf = 8)$p.value)
}



ggplot() + 
  geom_point(aes(x =9:20, y = lb, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = 9:20) + 
  scale_shape(name = "") + theme(legend.title = element_blank()) +
  xlab("Lag") + ylab("Ljung-Box p-values")

source("ARCH_TEST.R")

p1 = autoplot(acf(res^2)) + ggtitle("ACF of standardized residuals squared") +
  xlim(c(-0.5,25)) + ylim(c(-0.15,1)) + geom_segment(aes(x = 0,y=0,xend = 0,yend = 1))
p2 = autoplot(pacf(res^2)) + ggtitle("PACF of standardized residuals squared")
source("Multiplot.R")
multiplot(p1,p2,cols = 2)

p1


ea = c()
for(i in 1:11){
  ea = c(ea,ARCH_TEST(res,i)[1] )
}

ggplot() + 
  geom_point(aes(x =1:11, y = ea, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = 1:11) + 
  scale_shape(name = "") + theme(legend.title = element_blank()) +
  xlab("Lag") + ylab("ARCH LM p-values")



for(i in 9:20){
  Box.test(res,lag=i,type = "Ljung-Box",fitdf = 9)  
}


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


Predictions = OneStep( X = Deseasoned, Y = Daily18,AR = 3, MA = 2, arch = 1, garch = 2)
# write.table(Predictions, "C:/Users/Thomas/Dropbox/P6/R-dokumenter/Kode til Kap 7/Fix_Pred.txt",
#   sep="\t")
Predictions = read.csv("Fix_Pred.txt",sep = "\t")

RMSE = sqrt(mean((Predictions$Predictions-Daily18)^2)); RMSE

sum(Predictions$Upper > Daily18 & Predictions$Lower < Daily18)/length(Predictions$Lower)

p1 = ggplot() + 
  geom_line(aes( x = c(Daily2017$X__1[300:365],Daily2018$X__1) ,
                 y = ts( c(Deseasoned[300:364], Testset ) , start = 1 ) , 
                 colour = "Observed values") ) +
  geom_line(aes( x = c(Daily2017$X__1[365],Daily2018$X__1) ,
                 y = ts ( c(Deseasoned[365], Predictions$Predictions) ,
                          start = 365 ) , colour = "Predictions" ) ) +
  geom_ribbon(aes(x = Daily2018$X__1
                  , ymin = Predictions$Lower , ymax = Predictions$Upper) ,
              alpha = 0.2 , fill = "blue") +
  theme(legend.title = element_blank() ) + xlab("Time") +
  ylab("Deseasoned and demeaned prices") +
  scale_color_manual(values = c("black", "red"))

MAE_OneStep = mean( abs(Daily18 - Predictions$Predictions ) )
















