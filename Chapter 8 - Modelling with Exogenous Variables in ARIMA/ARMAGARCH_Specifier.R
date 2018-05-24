

Garch_Specifier = function(data,garch_order = c(2,2),arma_order = c(3,3),
                           dist = 'norm',method = "hybrid", Mean = FALSE, arma = T,xreg = NULL){
  #browser()
  Best_AIC = Inf
  n = length(data)
  xreg = as.matrix(xreg)
  
  if(arma){
    for(i in 1:garch_order[1]){
      for(j in 0:garch_order[2]){
        for(p in 0:arma_order[1]){
          for(q in 0:arma_order[2]){
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


#Old pure garch
{
  # else{
  #   for(i in 1:garch_order[1]){
  #     for(j in 0:garch_order[2]){
  #       spec = ugarchspec(mean.model = list(armaOrder=NULL,include.mean = Mean) ,
  #                         variance.model = list(model = 'sGARCH', garchOrder = c(i, j)), 
  #                         distribution = dist)
  #       Model = ugarchfit(spec,data, solver = method)
  #       AIC = infocriteria(Model)[2]
  #       if(AIC < Best_AIC){
  #         Best_AIC = AIC
  #         Best_Pair = c(i,j)
  #         Best_Model = Model
  #       }
  #       
  #     }
  #   }
  #   cat("GARCH order =", Best_Pair, sep = "\t")
  # }
}

