#install.packages("ggplot2")
library("ggplot2")
source("Multiplot.r")

set.seed(333)

X = seq(1,100)

Y1 = rep(0,100) #Simulating a random walk
for(i in 1:(length(X)-1)){ Y1[i+1] = Y1[i] + rnorm(1,0,1.1)}

df  = data.frame(X,Y1)

set.seed(711)

delta = 0.4 #Drift

Y2 = rep(0,100) # Simulating random walk with drift
for(i in 1:(length(X)-1)){ Y2[i+1] = delta + Y2[i] + rnorm(1,0,1.1)}

df  = data.frame(X,Y2)


RWD = ggplot(df, aes(X,Y2)) + geom_line() + xlab("Time") + ylab(expression(X[t])); RWD


multiplot(RW,RWD, cols =2) #Plotting
