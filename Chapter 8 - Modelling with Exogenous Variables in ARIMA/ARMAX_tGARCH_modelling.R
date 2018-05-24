
rm(list=ls())
getwd()

library(rugarch)
library(DescTools)
source("Expanded_sorting.r")
source("Multiplot.R")

#Creating and scaling design matrix for exogenous variables.
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
Ekso = Ekso/10000 ; Ekso18 = Ekso18/10000


#Deseasoning and partitioning data
DK1 = ts(Daily2017$DK1 - mean(Daily2017$DK1), frequency = 7)
STL_DK1 = stl(DK1 , s.window = "periodic")
Deseasoned = ts(DK1 - STL_DK1$time.series[,1], frequency = 1)
Daily18 = ts(Daily2018$DK1 - mean(Daily2017$DK1),start = 366,frequency = 1)
season = STL_DK1$time.series[,1]
Daily18 = Daily18 - season[2:(length(Daily18)+1)]
Data = ts(c(Deseasoned,Daily18))
Testset = ts ( c( Deseasoned[365] , Daily18) , start = 365  ) 



#Specifying and estimating model
source("ARMAGARCH_Specifier.R")
start.time <- Sys.time()
ARMAXGARCH = Garch_Specifier(Deseasoned,arma_order = c(5,5),
                            garch_order = c(2,2),Mean = FALSE,arma = T,xreg = Ekso, dist = "std")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Order was Arma(4,4) and ARCH(2,0)
spec = ugarchspec(mean.model = list(armaOrder = c(4,4),include.mean = FALSE,
                                    external.regressors = as.matrix(Ekso)),
                  variance.model = list(model = 'sGARCH',
                                        garchOrder = c(2,0)), 
                  distribution = "std",)
ARMAXGARCH = ugarchfit(spec,Data,solver = "hybrid",out.sample = 81)
infocriteria(ARMAXGARCH)
fit = ARMAXGARCH@fit
shape = as.numeric(fit$coef[length(fit$coef)])

#Model diagnostics:
res = fit$residuals/fit$sigma
checkresiduals(res)
Box.test(res,lag = 15,type = c("Ljung-Box"),fitdf = 13)
jarque.bera.test(res)

lb=c()
for (i in 15:25){
  lb = c(lb, Box.test(res,lag = i,type = c("Ljung-Box"), fitdf = 14)$p.value)
}

ggplot() + 
  geom_point(aes(x = 15:25, y = lb, shape = "p-value")) + 
  geom_hline(aes(yintercept = 0.05, color = "0.05")) +
  scale_x_discrete(limits = 15:25) + scale_y_continuous(limits = 0:1)
  scale_shape(name = "") + theme(legend.title = element_blank()) +
  xlab("Lags") + ylab("Ljung-Box p-values")


x <- seq(-6, 6, length=150)
hx <- dt(x,df = shape)


ggplot() +
  geom_histogram(aes(res),bins = 50,color = "black",fill = "grey") +
  geom_line(aes(x = x,y = hx*100, color = "t-density")) +
  scale_y_continuous(sec.axis = sec_axis(~.*1/100, name = "Density")) + ylab("Count")+
  theme(legend.title = element_blank() ) + xlab("Value") + scale_color_manual(values = c("red"))+
  guides(color=FALSE) + ggtitle("Histogram of standardized residuals and t-density curve")


p1 = autoplot(acf(res)) + ggtitle("ACF of standardized residuals") #+
#xlim(c(-0.5,25)) + ylim(c(-0.15,1)) + geom_segment(aes(x = 0,y=0,xend = 0,yend = 1))


multiplot(plot,p1)

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



