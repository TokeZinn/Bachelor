

fixed_Garch_Specifier = function(data,garch_order = c(2,2),arma_order = c(3,3), debug = F,
                           dist = 'norm',method = "hybrid", Mean = FALSE, arma = T,xreg = NULL){
  if(debug){
    browser()
  }
  Best_AIC = Inf
  n = length(data)
  if(is.null(xreg) == F){
    xreg = as.matrix(xreg)
  }
  p = arma_order[1] ; q = arma_order[2]
  if(arma){
    for(i in 1:garch_order[1]){
      for(j in 0:garch_order[2]){
        
            spec = ugarchspec(mean.model = list(armaOrder = c(p,q),include.mean = Mean,
                                                external.regressors = xreg) ,
                              variance.model = list(model = 'sGARCH', garchOrder = c(i, j)), 
                              distribution = dist)
            Model = ugarchfit(spec,data, solver = method)
            AIC = infocriteria(Model)[1]
            if(AIC < Best_AIC){
              Best_AIC = AIC
              Best_Pair = c(i,j)
              Best_Pair2 = c(p,q)
              Best_Model = Model
            }
        
      }
    }
    cat("GARCH order =", Best_Pair, "ARMA order = ", Best_Pair2,sep = "\t")
  }
  else{
    for(i in 1:garch_order[1]){
      for(j in 0:garch_order[2]){
        Model = garch(x = data,order = c(i,j),trace = F)
        AIC = -Model$n.likeli/length(data)
        if(AIC < Best_AIC){
          Best_AIC = AIC
          Best_Pair = c(i,j)
          Best_Model = Model
        }
      }
    }
  }
  
  cat("\n")
  return(Best_Model)
  
}

