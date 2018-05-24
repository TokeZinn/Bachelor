


rm(list=ls()) #Clear all
source("Expanded_sorting.R")

library(rugarch)
library(tseries)
library(forecast)
library(fracdiff)

#Deseasoning data
DK1 = ts(Daily2017$DK1 - mean(Daily2017$DK1), frequency = 7)
STL_DK1 = stl(DK1 , s.window = "periodic")
season = STL_DK1$time.series[,1]
Deseasoned = ts(DK1 - STL_DK1$time.series[,1], frequency = 1)



#Specifying and fitting the model
source("ARMAGARCH_Specifier.R")
ARMAGARCH = Garch_Specifier(Deseasoned,arma_order = c(3,5),
         garch_order = c(2,2),Mean = FALSE,arma = T)
#Result of above was 2,3,2,2
Arma_garch = Garch_Specifier(Deseasoned,arma_order = c(3,3),
                           garch_order = c(2,2),Mean = FALSE,arma = T)
spec = ugarchspec(mean.model = list(armaOrder = c(2,3),include.mean = FALSE),
                  variance.model = list(model = 'sGARCH',
                                        garchOrder = c(2,2)), distribution = "norm")
Arma_garch = ugarchfit(spec,Deseasoned, solver = "hybrid")

infocriteria(ARMAGARCH)
infocriteria(Arma_garch)
Arma_garch@fit$coef
fit = Arma_garch@fit$fitted.values

ggplot() + geom_line(aes(x = Daily2017$X__1, Deseasoned,color = "Observed values")) +
  geom_line(aes(x = Daily2017$X__1,fit,color = "Fitted values")) +
  xlab("Time") + ylab("Deseasoned and demeaned prices") + 
  scale_color_manual(values = c("blue", "red")) + theme(legend.title = element_blank() )


#Model diagnostics
res = Arma_garch@fit$residuals/Arma_garch@fit$sigma
checkresiduals(res)
jarque.bera.test(res)
plot(res)
idx = c()
for(i in 1:length(res)){
  if(abs(res[i])>2.6){
    idx = c(idx,i)
  }
}

idx1 = c()
for(i in 1:length(res)){
  if(abs(res[i])>2.75){
    idx1 = c(idx1,i)
  }
}

res2 = res[-idx];res3 = res[-idx1]
jarque.bera.test(res2) ; jarque.bera.test(res3)

# 11 largest cause the jarque bera test to fail. Without them the rest is accepted.

kurt = mean((res-mean(res))^4)/(mean((res-mean(res))^2))^2
skewness = mean((res-mean(res))^3)/(mean((res-mean(res))^2))^(3/2)

source("ARCH_TEST.R")

p1 = autoplot(acf(res^2)) + ggtitle("ACF of standardized residuals squared") +
  xlim(c(-0.5,25)) + ylim(c(-0.15,1)) + geom_segment(aes(x = 0,y=0,xend = 0,yend = 1))
p2 = autoplot(pacf(res^2)) + ggtitle("PACF of standardized residuals squared")
source("Multiplot.R")
multiplot(p1,p2,cols = 2)

for(i in 1:11){
  ARCH_TEST(res,i)  
}

for(i in 9:20){
  Box.test(res,lag=i,type = "Ljung-Box",fitdf = 9)  
}

lb=c()
for (i in 10:20){
  lb = c(lb, Box.test(res,lag = i,type = c("Ljung-Box"), fitdf = 9)$p.value)
}

ggplot() + 
  geom_point(aes(x =10:20, y = lb, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = 10:20) + 
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
  
