# BOOTSTRAP Prediction Intervals: 
source("Expanded_sorting.r")
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

ARMAXGARCH_tgarch = function(X,Y, ar = 1, ma = 1, arch = 1, garch = 1, xreg, xreg_out, dist = "norm", B = 100, P = NULL, Debug = F){
  if(Debug){
    browser()  
  }
  library(rugarch);library(forecast);library(tseries)
  Mean_Prediction = list()
  Sigma_Prediction = list()
  Upper = list()
  Lower = list()
  
  # initialization 
  xreg = as.matrix(xreg)
  xreg_out = as.matrix(xreg_out)
  
  #Length of in and out sample
  n = length(X)
  m = length(Y)
  
  #redefinition 
  series = c(X,Y)
  U = as.matrix(rbind(xreg,xreg_out))
  rownames(U) = 1:length(U[,1])
  
  xreg_out = as.data.frame(xreg_out)
  
  # Constructing Models
  spec = ugarchspec(mean.model = list(armaOrder = c(ar,ma),include.mean = FALSE,
                                      external.regressors = U),
                    variance.model = list(model = 'sGARCH',
                                          garchOrder = c(arch,garch)), 
                    distribution = dist)
  
  for (i in 1:m){
    t_start = Sys.time()
    X_i = series[-((n+i):(n+m))]
    U_i = U[-((n+i):(n+m)),]
    
    object = ugarchfit(spec,series, solver = "hybrid",out.sample = m-i+1)
    fit = object@fit
    coef = fit$coef 
    std_res = as.vector(fit$res/fit$sigma)
    forecast = ugarchforecast(object,n.ahead = 1,
                              n.roll = 0)
    
    arma_fit = Arima(X_i,order = c(ar,0,ma),include.mean = F, 
                     xreg = U_i,
                     fixed = c(coef[1:(ar+ma+length(xreg[1,]))]))
    
    Mean_Prediction[[i]] = forecast(arma_fit, h = 1, xreg = as.matrix(xreg_out[i,]))$mean
    Sigma_Prediction[[i]] = forecast@forecast$sigmaFor
    
    
    
    
    Upper[[i]] = quantile(drop(as.vector(Sigma_Prediction[[i]]))*std_res + drop(as.vector(Mean_Prediction[[i]])),0.975)
    Lower[[i]] = quantile(drop(as.vector(Sigma_Prediction[[i]]))*std_res + drop(as.vector(Mean_Prediction[[i]])),0.025)
    
    t_start = Sys.time() - t_start
    
    print(paste("Prediction", i, "of",m,"took", round(t_start,3),"minutes",sep = " "))
    
  }
  return(list("Prediction" = Mean_Prediction, "Sigma" = Sigma_Prediction, "Upper" = Upper, "Lower" = Lower))
}

Result_Boot = ARMAXGARCH_tgarch(X = Deseasoned,Y = Daily18,ar = 4, ma = 4, arch = 2,garch = 0,xreg = Ekso,xreg_out = Ekso18,dist = "std",Debug = T)


write.table(cbind(unlist(Result_Boot$Prediction),unlist(Result_Boot$Sigma),unlist(Result_Boot$Upper),unlist(Result_Boot$Lower)), "Predictions_Naiv.txt",
            sep="\t")



Intervals = sum(unlist(Result_Boot$Upper) > Daily18 & unlist(Result_Boot$Lower) < Daily18)/length(unlist(Result_Boot$Lower))

MAE_OneStep = mean( abs(Daily18 - unlist(Result_Boot$Prediction) ) )
