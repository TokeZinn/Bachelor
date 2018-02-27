# Sample Data -------------------------------------------------------------

model_structure = list(ar = c(0.8),
                       ma = c(1))
X = arima.sim(n = 100, model = model_structure)

Box.test(X,type = "Ljung-Box")
DFtest

# Other Functions ---------------------------------------------------------

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


# Descriptive Statistics Function -----------------------------------------

Descriptive_Plots = function(timeseries, dates, lags, draw = TRUE){
  library(stats);library(tseries);library(qwraps2);library(ggplot2);library(grid);library(gridExtra)
  if (missing(dates)){
    dates = 1:length(timeseries)
  }
  Data = data.frame(dates,timeseries)
  Plot_Data = ggplot(Data, aes(dates, timeseries)) + geom_line(colour="#211a52") + xlab("") + ylab("Price")  
  Plot_QQ = ggplot(Data, aes(sample = timeseries)) + stat_qq(colour = "#211a52") + ylab("Sample") + xlab("Theoretical")
  Plot_ACF = ggacf(timeseries)
  Plot_Hist = ggplot(Data, aes(timeseries)) + geom_histogram(fill ="white" ,col = "#211a52",bins = 50) + 
    xlab ("Price") + ylab("Observations") 
  grid.arrange(Plot_Data, Plot_QQ, Plot_ACF, Plot_Hist, nrow = 2, ncol=2)
}

Descriptive_Plots(X2017$DK1, X2017$X__1, 1)
