rm(list=ls())
source("Expanded_sorting.R")

Prognoses = read_excel("Data/Prognosis17.xlsx"); Prognoses = as.data.frame(Prognoses[,-(1:2)])

#Calculating daily prognoses
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
Prognoses_daily = HoursToDaysTransformer(Prognoses)

#Reading data, and removing data from other regions.
prod = read_excel("Data/2017_production.xlsx", skip = 2); prod = na.omit(prod);prod = prod[,-c(1:2,4:5)]
cons = read_excel("Data/2017_consumption.xlsx", skip = 2); cons = na.omit(cons);cons = cons[,-c(1:2,4:5)]
wind = read_excel("Data/2017_wind_power.xlsx", skip = 2); wind = na.omit(wind);wind = wind[,-c(1:2,4)]

#Collecting actual data
Actual = as.data.frame(cbind(prod,cons,wind));colnames(Actual) = c("Production","Consumption","WindPower")


Actual_daily = read_excel("Data/Daily_Ekso2017.xlsx"); Actual_daily = as.data.frame(Actual[,-(3:4)])
Actual[,1] = Actual[,1]-Actual[,3]


#Plotting prognoses vs actual data.
Wind = ggplot(data = Actual,aes(x = Actual$WindPower, y = Prognoses$WindPowerPrognosis)) + geom_point() +
  labs(x = "Wind Power", y = "Wind Prognosis") + ggtitle("(A)") +
  geom_abline(slope =1, intercept = 0, color = "red",size=1) +
  geom_smooth(method = "lm")


Prod = ggplot(data = Actual,aes(x = Actual$Production, y = Prognoses$ProductionPrognosis)) + 
  geom_point()+geom_smooth(method = "lm") + 
  geom_abline(slope =1, intercept = 0, color = "red",size=1) +
  labs(x = "Production", y = "Production Prognosis") + ggtitle("(C)")


Cons = ggplot(data = Actual,aes(x = Actual$Consumption, y = Prognoses$ConsumptionPrognosis)) + 
  geom_point()+geom_smooth(method = "lm") +
  geom_abline(slope =1, intercept = 0, color = "red",size=1) +
  labs(x = "Consumption", y = "Consumption Prognosis") + ggtitle("(B)")
source("Multiplot.R")
multiplot(Wind,Cons,Prod,cols = 3)



MAE = function(x,y){
  mae = sum(abs(x-y))/length(x)
  return(mae)
}
RMSE = function(x,y){
    rmse = sqrt(sum((x-y)^2)/length(x))
    return(rmse)
}
MAPE = function(x,y){
  mape = 100/length(x)*sum(abs((x-y)/y))
  cat("MAPE =", mape,sep = "\t")
}




sqrt(var(Actual$Production - Prognoses$ProductionPrognosis))
sqrt(var(Actual$Consumption - Prognoses$ConsumptionPrognosis))
sqrt(var(Actual$WindPower - Prognoses$WindPowerPrognosis))

MAE(Prognoses$WindPowerPrognosis,Actual$WindPower)
MAE(Actual$Consumption,Prognoses$ConsumptionPrognosis)
MAE(Actual$Production,Prognoses$ProductionPrognosis)

RMSE(Prognoses$WindPowerPrognosis,Actual$WindPower)
RMSE(Actual$Consumption,Prognoses$ConsumptionPrognosis)
RMSE(Actual$Production,Prognoses$ProductionPrognosis)

MAPE(Prognoses$WindPowerPrognosis,Actual$WindPower)
MAPE(Prognoses$ConsumptionPrognosis,Actual$Consumption)
MAPE(Prognoses$ProductionPrognosis,Actual$Production)




