# ACF plot in ggplot2  ---------------------------------------------------------

#Note that this code is heavily based on the following code:
#https://github.com/dewittpe/qwraps2/blob/master/R/qacf.R#L79
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


# Descriptive Plots Function -----------------------------------------

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

# Descriptive Statistics Function ------------------------------------

Descriptive_Statistics = function(timeseries, lags, alpha = 0.05, tex = F){
  library(tseries); library(stats)
  
  #Augmented Dickey Fueller Test: 
  ADF = adf.test(timeseries)
  Statistics = c(round(ADF$statistic[[1]],3))
  Pvalue = c(round(ADF$p.value[[1]],3))
  Names = c("Augmented Dickey Fueller")
  if (ADF$p.value[[1]]>alpha){
    Reject = c("No")
  }else{
    Reject = c("Yes")
  }
  
  #Jarque-Bera Test
  JB = jarque.bera.test(timeseries)
  Statistics = c(Statistics, round(JB$statistic[[1]],3))
  Pvalue = c(Pvalue, round(JB$p.value[[1]],3))
  Names = c(Names, "Jarque-Bera Test")
  if (JB$p.value[[1]]>alpha){
    Reject = c(Reject, "No")
  }else{
    Reject = c(Reject, "Yes")
  }
  
  
  #Ljung-Box test
  LB_result = list()
  for (i in lags){
    LB_result[[paste("Lag",i,sep = " ")]] = Box.test(timeseries, lag = i, type = "Ljung-Box")
    Names = c(Names, paste("Ljung-Box lag:",i,sep = " "))
  }
  for (j in LB_result){
    Statistics = c(Statistics, round(j$statistic[[1]],3))
    Pvalue = c(Pvalue, round(j$p.value[[1]],3))
    if (j$p.value[[1]]>alpha){
      Reject = c(Reject, "No")
    }else{
      Reject = c(Reject, "Yes")
    }
  }
  
  Data_Table = data.frame('Test-Type' = Names, 'Statistic' = Statistics, 'p-value' = Pvalue, 'Reject Null' = Reject)
  
  if (tex == T){
    print(xtable(Data_Table))
  }
  
  return(Data_Table)
}




