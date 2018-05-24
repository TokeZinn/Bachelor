library(tseries) # Load time series package 
library(readxl) # Load package to read Excel files

ELSPOT <- read_excel("Dropbox/P6/R-dokumenter/Discussion - stationarity/ELSPOT.xlsx") # Read the Excel file
elspot = ts(ELSPOT$DK1) # Make it a time series

plot(elspot) # Quick plot of the time series
adf.test(elspot, k = 0) # ADF-test of the 2013-2017 daily average prices

# The below shows the plot and test with the data on 06-07-13 adjusted (not important)
elspot[158] = mean(c(elspot[156:157], elspot[159:160])) 
plot(elspot)
adf.test(elspot, k = 0)
