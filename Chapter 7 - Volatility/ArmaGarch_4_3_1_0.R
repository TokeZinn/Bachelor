


rm(list=ls()) #Clear all
source("Expanded_sorting.R")

library(rugarch)
library(tseries)
library(forecast)
library(fracdiff)


DK1 = ts(Daily2017$DK1 - mean(Daily2017$DK1), frequency = 7)
STL_DK1 = stl(DK1 , s.window = "periodic")
season = STL_DK1$time.series[,1]
Deseasoned = ts(DK1 - STL_DK1$time.series[,1], frequency = 1)
Daily18 = ts(Daily2018$DK1 - mean(Daily2017$DK1),start = 366,frequency = 1)
Daily18 = Daily18 - season[2:(length(Daily18)+1)]
Data = ts(c(Deseasoned,Daily18))
Testset = ts ( c( Deseasoned[365] , Daily18) , start = 365  ) 

source("ARMAGARCH_Specifier.R")

ARMAGARCH = Garch_Specifier(Deseasoned,arma_order = c(5,5),debug = F,
                            garch_order = c(2,2),Mean = FALSE,arma = T)

#Result of above was 2,3,2,2
Arma_garch = Garch_Specifier(Deseasoned,arma_order = c(5,5),debug = F,
                             garch_order = c(2,2),Mean = FALSE,arma = T)
spec = ugarchspec(mean.model = list(armaOrder = c(4,3),include.mean = FALSE),
                  variance.model = list(model = 'sGARCH',
                                        garchOrder = c(1,0)), distribution = "norm")
Arma_garch = ugarchfit(spec,Deseasoned, solver = "hybrid")

infocriteria(Arma_garch)
Arma_garch@fit$coef
fit = Arma_garch@fit$fitted.values
Arma_garch@fit$coef
ggplot() + geom_line(aes(x = Daily2017$X__1, Deseasoned,color = "Observed values")) +
  geom_line(aes(x = Daily2017$X__1,fit,color = "Fitted values")) +
  xlab("Time") + ylab("Deseasoned and demeaned prices") + 
  scale_color_manual(values = c("blue", "red")) + theme(legend.title = element_blank() )

res = Arma_garch@fit$residuals/Arma_garch@fit$sigma
checkresiduals(res)

jarque.bera.test(res)
# 

kurt = mean((res-mean(res))^4)/(mean((res-mean(res))^2))^2
skewness = mean((res-mean(res))^3)/(mean((res-mean(res))^2))^(3/2)

source("ARCH_TEST.R")

p1 = autoplot(acf(res^2)) + ggtitle("ACF of standardized residuals squared") +
  xlim(c(-0.5,25)) + ylim(c(-0.15,1)) + geom_segment(aes(x = 0,y=0,xend = 0,yend = 1))
p2 = autoplot(pacf(res^2)) + ggtitle("PACF of standardized residuals squared")
source("Multiplot.R")
multiplot(p1,p2,cols = 2)


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




library(ggplot2)
ggplot() + geom_point(aes(x = 8:20, y = lb)) + xlab("Lag") + ylab("Ljung-Box p-value") +
  geom_abline(mapping = 0.05)


plot1 = ggplot() + 
  geom_line(aes(x = Daily2017$X__1, y = Arma_garch@fit$sigma,color = "Sigma Estimates")) +
  geom_line(aes(x = Daily2017$X__1, y = (1/4)*Deseasoned+24, color = "Prices")) +
  theme(legend.title = element_blank() ) + xlab("Time") +
  ylab("Estimated standard deviation") +
  scale_color_manual(values = c("Sigma Estimates" = "red", "Prices"="black")) +
  scale_y_continuous(sec.axis = sec_axis(~.*4-(4*24), name = "Deseasoned and Demeaned Prices")) +
  theme(legend.position = c(0.1, 0.4))


plot1

ggplot() + 
  geom_line(aes(x = Daily2017$X__1, y = Arma_garch@fit$sigma^2,color = "Sigma Estimates")) +
  geom_line(aes(x = Daily2017$X__1, y = (1/4)*Deseasoned^2+24, color = "Prices")) +
  theme(legend.title = element_blank() ) + xlab("Time") +
  ylab("Estimated standard deviation") +
  scale_color_manual(values = c("Sigma Estimates" = "red", "Prices"="blue")) +
  scale_y_continuous(sec.axis = sec_axis(~.*4-(4*24), name = "Deseasoned and Demeaned Prices")) +
  theme(legend.position = c(0.1, 0.8))

plot2 = ggplot() + 
  geom_line(aes(x = Daily2017$X__1, y = Arma_garch@fit$sigma^2,color = "red")) +
  ylab("Estimated Variance") + xlab("Time") + scale_color_manual(values = c("red")) +
  theme(legend.title = element_blank() ,legend.position = "none")

plot3 = ggplot() + 
  geom_line(aes(x = Daily2017$X__1, y = Deseasoned^2)) +
  xlab(NULL) + ylab("Squared Prices")

plot3

multiplot(plot3,plot2,cols = 1)


OneStep = function( X, Y , AR = 5 , MA = 4, arch=2, garch=0,debug = F) {
  if(debug){
    browser()
  }
  All_Data = c( X, Y)
  n = length(X)
  m = length(Y)
  OneStepPredictions = rep(0,m)
  OneUpper = rep(0,m)
  OneLower = rep(0,m)
  OneSigma2 = rep(0,m)
  
  
  for (i in 1:m) {
    # if(i == 13){
    #   browser()
    # }
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
    
    X = as.matrix(cbind(X)) 
    xt = c(xt)
    
    se = hatsigma*sqrt(1+(t(xt)%*%(solve(t(X)%*%X))%*%xt))
    OneUpper[i] = qnorm(0.975)*se+
      OneStepPredictions[i]
    OneLower[i] = OneStepPredictions[i] + 
      qnorm(0.025)*se
    OneSigma2[i] = hatsigma^2
    
    cat("Iteration =",i,"\n",sep = "\t")
    
  }
  
  Output = data.frame("Predictions" = OneStepPredictions, "Upper" = OneUpper,
                      "Lower" = OneLower, "PredVariance" = OneSigma2)
  return(Output)
  
}
set.seed(1)
pred = OneStep( X = Deseasoned, Y = Daily18,AR = 4, MA = 3, arch = 1, garch = 0,debug = F)


# Export predictions, da ovenstående tager lang tid.
# write.table(pred, "C:/Users/Thomas/Dropbox/P6/R-dokumenter/Kode til Kap 7/Predictions_4_3_1_0.txt",
#             sep="\t")


pred = read.csv("Predictions_4_3_1_0.txt",sep = "\t")

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


