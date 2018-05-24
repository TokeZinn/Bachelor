rm(list=ls()) #Clear all
source("Expanded_sorting.R")
source("ARCH_TEST.R")

Ekso = read_excel("Data/Prognosis17.xlsx"); Ekso = as.data.frame(Ekso[,-(1:2)])/10000

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

Ekso = HoursToDaysTransformer(Ekso)

  
DK1 = ts(Daily2017$DK1 - mean(Daily2017$DK1), frequency = 7)

STL_DK1 = stl(DK1 , s.window = "periodic")

Deseaonel_DK1 = ts(DK1 - STL_DK1$time.series[,1], frequency = 1)

Model_Specifier_ekso = function(data,p_max=5,q_max=5){
  Best_AIC = Inf
  
  for(i in 0:p_max) {
    for(j in 0:q_max){
      model = arima(data , order = c(i,0,j) , include.mean = FALSE , xreg = Ekso ,method = "ML")
      AIC_temp = model$aic
      
      if( AIC_temp < Best_AIC){
        Best_AIC = AIC_temp
        Best_pair = c(i,j)
      }
    }
  }
  return(Best_pair)
}

Pair_ekso = Model_Specifier_ekso(Deseaonel_DK1) #5,4

model_ekso = arima(Deseaonel_DK1, order = c(Pair_ekso[1],0,Pair_ekso[2]) , xreg = Ekso , 
                   include.mean = FALSE, method ="ML")

fitted_arima = fitted.values(model_ekso)

df  = data.frame(Daily2017$X__1, Deseaonel_DK1 , fitted_arima)

ggplot(df, aes(Daily2017.X__1)) + geom_line(aes(y =  Deseaonel_DK1 , colour = "Observed Values")) + 
  geom_line(aes(y = fitted_arima , colour = "Fitted values") ) + xlab("Time") + ylab("Deseaoned and demeaned prices") +
  scale_color_manual(values = c("blue", "red")) + theme(legend.title = element_blank() )

#Diagnostics
checkresiduals(model_ekso$residuals)
jarque.bera.test(model_ekso$residuals)
res = model_ekso$residuals

lb=c() ;fitdf = 12 ; l = fitdf+1; ub = l+9
for (i in l:ub){
  lb = c(lb, Box.test(res,lag = i,type = c("Ljung-Box"), fitdf = fitdf)$p.value)
}


ggplot() + 
  geom_point(aes(x =l:ub, y = lb, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = l:ub) + scale_y_continuous(limits = 0:1) +
  scale_shape(name = "") + theme(legend.title = element_blank()) +
  xlab("Lag") + ylab("Ljung-Box p-values")

for( i in 1:10){
ARCH_TEST(model_ekso$residuals , lags = i) }

p1 = autoplot(acf(res^2)) + ggtitle("ACF of standardized residuals squared") +
  xlim(c(-0.5,25)) + ylim(c(-0.15,1)) + geom_segment(aes(x = 0,y=0,xend = 0,yend = 1))
p2 = autoplot(pacf(res^2)) + ggtitle("PACF of standardized residuals squared")
source("Multiplot.R")
multiplot(p1,p2,cols = 2)

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


for( i in 13:20) {
print(Box.test(model_ekso$residuals , lag = i , fitdf = 12)$p.value) }




#Forecast
Season = ts(STL_DK1$time.series[,1], frequency = 1)

Season2018 = Season[2:82]

Testset = ts ( c( Deseaonel_DK1[365] , Daily2018$DK1 - Season2018 - mean(Daily2017$DK1)) , start = 365  ) 

Ekso18 = read_excel("Data/Prognosis18.xlsx"); Ekso18 = HoursToDaysTransformer(Ekso18[,-(1:2)])/10000


OneStep_ekso = function( X, Y , Ekso_Train, Ekso_Test, AR = 5 , MA = 4) {
  
  #browser()
  
  All_Data = c( X, Y)
  All_Ekso = as.data.frame(rbind(as.matrix(Ekso_Train), as.matrix(Ekso_Test)))
  
  n = length(X)
  m = length(Y)
  
  OneStepPredictions = rep(0,m)
  OneUpper = rep(0,m)
  OneLower = rep(0,m)
  
  for (i in 1:m) {
    
    Current_Data = All_Data[-((n+i):(n+m))]
    Current_Ekso = All_Ekso[-((n+i):(n+m)),]
    
    Current_fit = Arima(Current_Data , order = c(AR,0,MA) , include.mean = FALSE , xreg = Current_Ekso , method = "ML")
    
    Current_forecast = forecast(Current_fit, h = 1, xreg = Ekso_Test[i,] )
    
    OneStepPredictions[i] = Current_forecast$mean 
    
    OneUpper[i] = Current_forecast$upper
    
    OneLower[i] = Current_forecast$lower
    
  }
  
  
  Output = data.frame("Predictions" = OneStepPredictions , "Upper" = OneUpper , "Lower" = OneLower)
  
  return(Output)
  
}

OneStepPredictions = OneStep_ekso(Deseaonel_DK1, Testset[-1] , Ekso, Ekso18)

write.table(OneStepPredictions, "Predictions_5_4.txt",
                        sep="\t")
OneStepPredictions = read.csv("Predictions_5_4.txt",
                          sep="\t")

RMSE = sqrt(mean((OneStepPredictions$Predictions-Daily18)^2)); RMSE


mean( abs(Testset[2:82] - forecast(model_ekso , xreg = Ekso18)$mean ) )
mean( abs(Testset[2:82] - OneStepPredictions$Predictions ) )

ggplot() + 
  geom_line(aes( x = c(Daily2017$X__1[300:365],Daily2018$X__1) , y = ts( c(Deseaonel_DK1[300:364], Testset ) , start = 300 ) , colour = "Observed values") ) +
  geom_line(aes( x = c(Daily2017$X__1[365],Daily2018$X__1) , y = ts ( c(Deseaonel_DK1[365], OneStepPredictions$Predictions) , start = 365 ) , colour = "Predictions" ) ) +
  geom_ribbon(aes(x = Daily2018$X__1, ymin = OneStepPredictions$Lower , ymax = OneStepPredictions$Upper) , alpha = 0.2 , fill = "blue") +
  theme(legend.title = element_blank() ) + xlab("Time") + ylab("Deseasoned and demeaned prices") +
  scale_color_manual(values = c("black", "blue"))


Intervals = sum(OneStepPredictions$Upper > Testset[-1] & OneStepPredictions$Lower < Testset[-1])/length(OneStepPredictions$Lower)
