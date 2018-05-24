library(readxl)
library(tseries)

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

Ekso = read_excel("1.Long_Ekso.xlsx") 

Ekso_Days = HoursToDaysTransformer(Ekso[,(2:4)])/10000

plot(ts(Ekso_Days, start = c(2013,1) , frequency = 365))

adf.test(ts(Ekso_Days[,1]))
adf.test(ts(Ekso_Days[,2]))
adf.test(ts(Ekso_Days[,3]))

