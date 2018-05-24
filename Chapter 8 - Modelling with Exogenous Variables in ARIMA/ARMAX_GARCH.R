## Initialization ----
rm(list=ls())
library(rugarch)
library(DescTools)
source("Expanded_sorting.r")
getwd()
Ekso = read_excel("Data/Prognosis17.xlsx"); Ekso = as.data.frame(Ekso[,-(1:2)])
Ekso18 = read_excel("Data/Prognosis18.xlsx"); Ekso18 = as.data.frame(Ekso18[,-(1:2)])

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

Ekso = HoursToDaysTransformer(Ekso) ; Ekso18 = HoursToDaysTransformer(Ekso18)
Ekso = Ekso/10000; Ekso18 = Ekso18/10000;

DK1 = ts(Daily2017$DK1 - mean(Daily2017$DK1), frequency = 7)
STL_DK1 = stl(DK1 , s.window = "periodic")

Deseasoned = ts(DK1 - STL_DK1$time.series[,1], frequency = 1)

Daily18 = ts(Daily2018$DK1 - mean(Daily2017$DK1),start = 366,frequency = 1)
season = STL_DK1$time.series[,1]
Daily18 = Daily18 - season[2:(length(Daily18)+1)]

Data = ts(c(Deseasoned,Daily18))
Testset = ts ( c( Deseasoned[365] , Daily18) , start = 365  ) 

## Specification (5,4)x(2,0) ---- 

source("ARMAGARCH_Specifier.R")

# start.time <- Sys.time()
# ARMAXGARCH = Garch_Specifier(Deseasoned,arma_order = c(5,5),
#                             garch_order = c(2,2),Mean = FALSE,arma = T,xreg = Ekso)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

# Order was Arma(5,4) and ARCH(2)

spec = ugarchspec(mean.model = list(armaOrder = c(5,4),include.mean = FALSE,
                                    external.regressors = as.matrix(Ekso)),
                  variance.model = list(model = 'sGARCH',
                                        garchOrder = c(2,0)), 
                  distribution = "norm")
ARMAXGARCH = ugarchfit(spec,Data,solver = "hybrid",out.sample = 81)
fit = ARMAXGARCH@fit
infocriteria(ARMAXGARCH)


## Plots: fit ---- 
library(ggplot2)
ggplot()+
  geom_line(aes(x = Daily2017$X__1, y = ts(Deseasoned,start = 1),colour = "Observed Values")) + 
  geom_line(aes(x = Daily2017$X__1, y = ts(fit$fitted.values), colour = "Estimated Values")) +
  theme(legend.title = element_blank() ) + xlab("Time") +
  ylab("Deseasoned and demeaned prices") +
  scale_color_manual(values = c("red", "blue"))

res = fit$residuals/fit$sigma
checkresiduals(res)

p1 = autoplot(acf(res^2)) + ggtitle("ACF of standardized residuals squared") +
  xlim(c(-0.5,25)) + ylim(c(-0.15,1)) + geom_segment(aes(x = 0,y=0,xend = 0,yend = 1))
p2 = autoplot(pacf(res^2)) + ggtitle("PACF of standardized residuals squared")
source("Multiplot.R")
multiplot(p1,p2,cols = 2)


lb = c()
for (i in 15:25){
  lb = c(lb, Box.test(res,lag = i,type = c("Ljung-Box"), fitdf = 14)$p.value)
}


ggplot() + 
  geom_point(aes(x = 15:25, y = lb, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = 15:25) + 
  scale_shape(name = "") + theme(legend.title = element_blank()) +
  xlab("Lags") + ylab("Ljung-Box p-values")


source("ARCH_TEST.R")
ea = c()
for(i in 1:11){
  ea = c(ea,ARCH_TEST(res,i,save = T)  )
}

ggplot() + 
  geom_point(aes(x = 1:11, y = ea, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = 1:11)+
  scale_shape(name = "") + theme(legend.title = element_blank()) +
  xlab("Lags") + ylab("ARCH-test p-values") + ylim(0,1)

  
## Prediction ---- 

OneStep = function( X, Y , AR = 5 , MA = 4, arch=2, garch=0,xreg,xreg_out) {
  #browser()
  #arch = 2;garch = 0 
  All_Data = c( X, Y)
  n = length(X)
  m = length(Y)
  OneStepPredictions = rep(0,m)
  OneUpper = rep(0,m)
  OneLower = rep(0,m)
  OneSigma2 = rep(0,m)
  xreg = as.matrix(xreg)
  xreg_out = as.matrix(xreg_out)
  All_Ekso = as.matrix(rbind(xreg,xreg_out))
  xreg_out = as.data.frame(xreg_out)
  for (i in 1:m) {
    Current_Data = All_Data[-((n+i):(n+m))]
    Current_Ekso = All_Ekso[-((n+i):(n+m)),]
      spec = ugarchspec(mean.model = list(armaOrder = c(AR,MA),include.mean = FALSE,
                                          external.regressors = All_Ekso),
                        variance.model = list(model = 'sGARCH',
                                              garchOrder = c(arch,garch)), 
                        distribution = "norm")
      object = ugarchfit(spec,All_Data, solver = "hybrid",out.sample = m-i+1)
      full_fit = object@fit
      coef = full_fit$coef ; res = full_fit$residuals
      arma_fit = Arima(Current_Data,order = c(AR,0,MA),include.mean = F, 
                       xreg = Current_Ekso,
                       fixed = c(coef[1:(AR+MA+length(xreg[1,]))]))
      
      len = length(Current_Data)
      forecast = ugarchforecast(object,n.ahead = 1,
                                n.roll = 0)
        
        hatsigma = forecast@forecast$sigmaFor
        OneStepPredictions[i] = forecast(arma_fit, h = 1,xreg = xreg_out[i,])$mean 
        
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
        
      X = cbind(X,Current_Ekso[MAX:(len-1),]) 
      xt = c(xt, as.matrix(xreg_out)[i,])
        
        # x1 = fitted[5:(len-1)]
        # x2 = lag(fitted)[5:(len-1)]
        # x3 = lag(lag(fitted))[5:(len-1)]
        # x4 = lag(lag(lag(fitted)))[5:(len-1)]
        # x5 = lag(lag(lag(lag(fitted))))[5:(len-1)]
        # 
        # x6 = res[5:(len-1)]
        # x7 = lag(res)[5:(len-1)]
        # x8 = lag(lag(res))[5:(len-1)]
        # x9 = lag(lag(lag(res)))[5:(len-1)]
        # X = as.matrix(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9))
        
      
        # 
        # xt = as.numeric(c(fitted[len],fitted[(len-1)],fitted[(len-2)],fitted[(len-3)],
        #                   fitted[(len-4)],res[length(res)],res[length(res)-1],
        #                   res[length(res)-2],res[length(res)-3]),xreg_out[i,])
        se = hatsigma*sqrt(1+(t(xt)%*%(solve(t(X)%*%X))%*%xt))
        OneUpper[i] = qt(0.975,len - (AR + MA + arch + garch + length(Current_Ekso[1,])))*se+
          OneStepPredictions[i]
        OneLower[i] = OneStepPredictions[i] + 
          qt(0.025,len - (AR + MA + arch + garch + length(Current_Ekso[1,])))*se
        OneSigma2[i] = hatsigma^2
        
        
        
      }
    
    Output = data.frame("Predictions" = OneStepPredictions, "Upper" = OneUpper,
                        "Lower" = OneLower, "PredVariance" = OneSigma2)
  return(Output)
  
}

start.time <- Sys.time()
Predictions = OneStep(X = Deseasoned, Y = Daily18, AR = 5, MA =4,arch =2, garch=0, xreg = Ekso, xreg_out = Ekso18)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#write.table(Predictions, "Predictions_5_4_x_2_0.txt",
#                         sep="\t")
Predictions = read.csv("Predictions_5_4_x_2_0.txt",sep = "\t")
RMSE = sqrt(mean((Predictions$Predictions-Daily18)^2)); RMSE
Intervals = sum(Predictions$Upper > Daily18 & Predictions$Lower < Daily18)/length(Predictions$Lower)
MAE_OneStep = mean( abs(Daily18 - Predictions$Predictions ) ) ; MAE_OneStep
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
p1


ARMAXGARCH@fit$coef

Intervals

MAE_OneStep = mean( abs(Daily18 - Predictions$Predictions ) )

## ARMAX_35x21 ---- 

spec = ugarchspec(mean.model = list(armaOrder = c(3,5),include.mean = FALSE,
                                    external.regressors = as.matrix(Ekso)),
                  variance.model = list(model = 'sGARCH',
                                        garchOrder = c(2,1)), 
                  distribution = "norm")
ARMAXGARCH = ugarchfit(spec,Data,solver = "hybrid",out.sample = 81)
fit = ARMAXGARCH@fit
infocriteria(ARMAXGARCH)

ggplot()+
  geom_line(aes(x = Daily2017$X__1, y = ts(Deseasoned,start = 1),colour = "Observed Values")) + 
  geom_line(aes(x = Daily2017$X__1, y = ts(fit$fitted.values), colour = "Estimated Values")) +
  theme(legend.title = element_blank() ) + xlab("Time") +
  ylab("Deseasoned and demeaned prices") +
  scale_color_manual(values = c("red", "blue"))

fit$coef
res = fit$residuals/fit$sigma
jarque.bera.test(res)
checkresiduals(res)

p1 = autoplot(acf(res^2)) + ggtitle("ACF of standardized residuals squared") +
  xlim(c(-0.5,25)) + ylim(c(-0.15,1)) + geom_segment(aes(x = 0,y=0,xend = 0,yend = 1))
p2 = autoplot(pacf(res^2)) + ggtitle("PACF of standardized residuals squared")
source("Multiplot.R")
multiplot(p1,p2,cols = 2)



lb = c()
for (i in 15:25){
  lb = c(lb, Box.test(res,lag = i,type = c("Ljung-Box"),fitdf = 14)$p.value )
}



Box.test(res,lag = 15,type = c("Ljung-Box"),fitdf = 14)



ggplot() + 
  geom_point(aes(x =15:25, y = lb, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = 15:25) + 
  scale_shape(name = "") + theme(legend.title = element_blank()) +
  xlab("lags") + ylab("Ljung-Box p-values")


source("ARCH_TEST.R")
ea = c() ; u = 11
for(i in 1:u){
  ea = c(ea,ARCH_TEST(res,i,save = T)  )
}

ggplot() + 
  geom_point(aes(x = 1:u, y = ea, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = 1:u)+
  scale_shape(name = "") + theme(legend.title = element_blank()) +
  xlab("Lags") + ylab("ARCH-test p-values") + ylim(0,1)

Predictions = OneStep(X = Deseasoned, Y = Daily18, AR = 3, MA =5,arch =2, garch=1, xreg = Ekso, xreg_out = Ekso18)



Intervals = sum(Predictions$Upper > Daily18 & Predictions$Lower < Daily18)/length(Predictions$Lower)

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
p1

Intervals

MAE_OneStep = mean( abs(Daily18 - Predictions$Predictions ) )

kurtosis(Daily18 - Predictions$Predictions)
skewness(Daily18-Predictions$Predictions)

plot(res, type = "l")

kurtosis(res)
skewness(res)

Predictions = read.csv("Predictions_3_5_x_2_1.txt", sep = "\t")
RMSE = sqrt(mean((Predictions$Predictions-Daily18)^2)); RMSE


