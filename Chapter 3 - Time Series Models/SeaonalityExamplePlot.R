
library(ggplot2) # Plotting library

set.seed(2) # Set seed for random number generator
beta = c(0.7,0.5,0.2) # parameter values
w = rep(0,59) 
x = rep(0,length(w))

for(i in 1:length(w)){ # initializes the w-vector with N(0,1) random values  
  w[i] = rnorm(1,0,1)
}

for(i in 9:length(x)){ # Creating the X_t values according to the equation 3.25
  x[i] = x[i-1]+x[i-4]-x[i-5] + w[i]+beta[1]*w[i-1]+beta[2]*w[i-4]
         +beta[3]*w[i-8]+beta[1]*beta[2]*w[i-5]+beta[1]*beta[3]*w[i-9]
}

diff_x = diff(x) # Creates a differenced series of X_t
for(j in 5:length(diff_x)){diff_x[j] = diff_x[j]-diff_x[j-4]}; diff_x

x = ts(x[19:length(x)], frequency = 4) # Makes the x-vector to a time series
diff_x = ts(diff_x[18:length(diff_x)]) # Makes the diff_x-vector to a time series

fit = stl(x, s.window = "periodic") # Performs the STL procedure
y = fit$time.series[,2] + fit$time.series[,3] # Combines the trend and remainder component (excluding the seasonal component from x)
diff_y = ts(diff(y)) # Creates a differenced time series of y

t = seq(1,length(x)) # Creates a time sequence

df = data.frame(x, diff_x, t, y) # Data frame of x, diff_x, y, and t 
df2 = data.frame(diff_y, t[-length(t)]) # Data frame of diff_y and t(adjusted in length)

ggplot(df, aes(t,x)) + geom_line() + geom_point() + xlab(expression(t)) + ylab(expression(X[t]))
#The above plots the X_t process. The below plots the differenced X_t process
ggplot(df, aes(t,diff_x)) + geom_line() + geom_point() + xlab(expression(t)) + ylab(expression(paste(nabla[4],nabla,X[t])))

ggplot(df, aes(t,y)) + geom_line() + geom_point() + xlab(expression(t)) + ylab(expression(Y[t]))
#The above plots the Y_t process (no seasonality). The below plots the differenced Y_t process
ggplot(df2, aes(df2$t,diff_y)) + geom_line() + geom_point() + xlab(expression(t)) + ylab(expression(paste(nabla,Y[t])))


