# Modelling daily Prices
rm(list=ls()) #Clear all
source("Expanded_sorting.r")

DK1 = ts(Daily2017$DK1 - mean(Daily2017$DK1), frequency = 7) #Demeaning
 

STL_DK1 = stl(DK1 , s.window = "periodic") #Identifying trend component.

Deseaonel_DK1 = ts(DK1 - STL_DK1$time.series[,1], frequency = 1) #Detrending

adf.test(Deseaonel_DK1) #Testing for stationarity. 

jarque.bera.test(Deseaonel_DK1)

#Making a plot

ggacf = function(x, conf_level = 0.95, type = "acf"){
  library(dplyr); library(stats); library(ggplot2);library(tidyr)
  if (type == "pacf"){
    acf_data = stats::pacf(x, plot = F)
  }else{
    acf_data = stats::acf(x, plot = F)
  }
  signif = stats::qnorm((1 - conf_level)/2)/sqrt(acf_data$n.used)
  lags = dplyr::as_data_frame(acf_data$lag)
  acfs = dplyr::as_data_frame(acf_data$acf)
  acf_df = dplyr::bind_cols(tidyr::gather(lags, key = 'key', value = 'lag'),
                            tidyr::gather(acfs, key = 'key', value = 'value')["value"])
  acf_df = dplyr::mutate(acf_df, Significant = factor(abs(.data$value) > abs(signif)))
  
  g = ggplot2::ggplot() + 
    ggplot2::aes_string(x = "lag", y = "value") +
    ggplot2::geom_bar(stat = "identity", position = "identity") +
    ggplot2::ylab("Correlation") + 
    ggplot2::xlab("Lag")+
    ggplot2::geom_hline(yintercept = signif) + 
    ggplot2::geom_hline(yintercept = -signif) + 
    ggplot2::aes_string(fill = "Significant") + 
    ggplot2::coord_cartesian(ylim = c(-1,1))
  g = ggplot2::`%+%`(g, acf_df)
  g
}

Descriptive_Plots = function(timeseries, dates, draw = TRUE){
  library(stats);library(tseries);library(ggplot2);library(grid);library(gridExtra)
  if (missing(dates)){
    dates = 1:length(timeseries)
  }
  
  #Theme = theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0)) +
  #  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold")) 
  
  Data = data.frame(dates,timeseries)
  Plot_Data = ggplot(Data, aes(dates, timeseries)) + geom_line(colour="#211a52") + xlab("") + ylab("Price") + ggtitle("Price vs. Time") 
  Plot_PACF = ggacf(timeseries, type ="pacf") + ggtitle("Partial Autocorrelation Plot")
  Plot_ACF = ggacf(timeseries) + ggtitle("Autocorrelation Plot") 
  Plot_Hist = ggplot(Data, aes(timeseries)) + geom_histogram(fill ="white" ,col = "#211a52",bins = 50) + 
    xlab ("Price") + ylab("Observations") + ggtitle("Histogram of Prices") 
  grid.arrange(Plot_Data, Plot_Hist, Plot_ACF, Plot_PACF , nrow = 2, ncol=2)
}

DK1_plot = ts(Daily2017$DK1, frequency = 1)
Descriptive_Plots(DK1_plot,Daily2017$X__1)
Descriptive_Plots(Deseaonel_DK1, Daily2017$X__1)

#Choosing the best model

Model_Specifier= function(data,p_max=5,q_max=5){
  Best_AIC = Inf
  
  for(i in 0:p_max) {
    for(j in 0:q_max){
      model = arima(data , order = c(i,0,j) , include.mean = FALSE)
      AIC_temp = model$aic
      
      if( AIC_temp < Best_AIC){
        Best_AIC = AIC_temp
        Best_pair = c(i,j)
      }
    }
  }
  return(Best_pair)
}

MS = Model_Specifier(Deseaonel_DK1)

model_ARIMA = arima(Deseaonel_DK1, order =c(MS[1],0,MS[2]) , include.mean = FALSE)

fitted_arima = fitted.values(model_ARIMA)

df  = data.frame(Daily2017$X__1, Deseaonel_DK1 , fitted_arima)

ggplot(df, aes(Daily2017.X__1)) + geom_line(aes(y =  Deseaonel_DK1 , colour = "Observed Values")) + 
  geom_line(aes(y = fitted_arima , colour = "Fitted values") ) + xlab("Time") + ylab("Deseaoned and demeaned prices") +
  scale_color_manual(values = c("blue", "red")) + theme(legend.title = element_blank() )

#Model diagnostics

res = model_ARIMA$residuals/sqrt(model_ARIMA$sigma2)

jarque.bera.test(res)

lb=c() ;fitdf = 5 ; l = fitdf+1; ub = l+9
for (i in l:ub){
  lb = c(lb, Box.test(res,lag = i,type = c("Ljung-Box"), fitdf = fitdf)$p.value)
}

ggplot() + 
  geom_point(aes(x =l:ub, y = lb, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = l:ub) + scale_y_continuous(limits = 0:1) +
  scale_shape(name = "") + theme(legend.title = element_blank()) +
  xlab("Lag") + ylab("Ljung-Box p-values")


source("ARCH_TEST.R")
ea = c()
for(i in 1:10){
  ea = c(ea,ARCH_TEST(res,i,save = T)  )
}

ggplot() + 
  geom_point(aes(x = 1:10, y = ea, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = 1:10)+
  scale_shape(name = "") + theme(legend.title = element_blank()) +
  xlab("Lags") + ylab("ARCH-test p-values") + ylim(0,1)


# Forecasting:

Season = ts(STL_DK1$time.series[,1], frequency = 1)

Season2018 = Season[2:82]

Testset = ts ( c( Deseaonel_DK1[365] , Daily2018$DK1 - Season2018 - mean(Daily2017$DK1)) , start = 365  ) 

forecast81 = forecast(object = model_ARIMA, h = 81 , level = 95)

MAE =  mean( abs(Testset[2:82] - forecast81$mean ) )

ggplot() + 
  geom_line(aes( x = c(Daily2017$X__1[300:365],Daily2018$X__1) , y = ts( c(Deseaonel_DK1[300:364], Testset ) , start = 300 ) , colour = "Observed values") ) +
  geom_line(aes( x = c(Daily2017$X__1[365],Daily2018$X__1) , y = ts ( c(Deseaonel_DK1[365], forecast81$mean) , start = 365 ) , colour = "Predictions" ) ) +
  #geom_line(aes( x = Daily2018$X__1 , y = ts ( forecast81$upper , start = 365 ) , colour = "Upper prediction interval" ) ) + 
  #geom_line(aes( x = Daily2018$X__1 , y = ts ( forecast81$lower , start = 365 ) , colour = "Lower prediction interval" ) ) +
  geom_ribbon(aes(x = Daily2018$X__1, ymin = forecast81$lower , ymax = forecast81$upper) , alpha = 0.2 , fill = "blue") +
  theme(legend.title = element_blank() ) + xlab("Time") + ylab("Deseasoned and demeaned prices") +
    scale_color_manual(values = c("black", "blue" ))

# One Step Forecast

OneStep = function( X, Y , AR = 3 , MA = 2) {
  
  #browser()
  
  All_Data = c( X, Y)
  
  n = length(X)
  m = length(Y)
  
  OneStepPredictions = rep(0,m)
  OneUpper = rep(0,m)
  OneLower = rep(0,m)
  
  for (i in 1:m) {
    
    Current_Data = All_Data[-((n+i):(n+m))]
    
    Current_fit = Arima(Current_Data , order = c(AR,0,MA) , include.mean = FALSE)
    
    OneStepPredictions[i] = forecast(Current_fit, h = 1)$mean 
    
    OneUpper[i] = forecast(Current_fit, h = 1)$upper
    
    OneLower[i] = forecast(Current_fit, h = 1)$lower
    
  }
  
  
  Output = data.frame("Predictions" = OneStepPredictions , "Upper" = OneUpper , "Lower" = OneLower)
  
  return(Output)
  
}

OneStepPredictions = OneStep(Deseaonel_DK1, Testset[-1])
write.table(OneStepPredictions, "OneStepPrediction.txt",
                        sep="\t")

ggplot() + 
  geom_line(aes( x = c(Daily2017$X__1[300:365],Daily2018$X__1) , y = ts( c(Deseaonel_DK1[300:364], Testset ) , start = 300 ) , colour = "Observed values") ) +
  geom_line(aes( x = c(Daily2017$X__1[365],Daily2018$X__1) , y = ts ( c(Deseaonel_DK1[365], OneStepPredictions$Predictions) , start = 365 ) , colour = "Predictions" ) ) +
  geom_ribbon(aes(x = Daily2018$X__1, ymin = OneStepPredictions$Lower , ymax = OneStepPredictions$Upper) , alpha = 0.2 , fill = "blue") +
  theme(legend.title = element_blank() ) + xlab("Time") + ylab("Deseasoned and demeaned prices") +
  scale_color_manual(values = c("black", "blue"))


MAE_OneStep = mean( abs(Testset[2:82] - OneStepPredictions$Predictions ) )
RMSE = sqrt(mean((OneStepPredictions$Predictions-Testset[2:82])^2)); RMSE

#Percent of points inside prediction intervals.
sum(OneStepPredictions$Upper > Daily18 & OneStepPredictions$Lower < Daily18)/length(OneStepPredictions$Lower)
