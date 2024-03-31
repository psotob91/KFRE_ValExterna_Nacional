# Funcion que recalcula supervivencia basal usando Competing risk model
recal.risk.basal.csh <- function(data, horizon) {
  
  fc.aj.risk.basal <- rep(NA, max(data$.imp))
  
  for (i in 1:max(data$.imp)) {
    
    datos_filter <- data |> 
      filter(.imp == i) 
    
    datos_filter <- datos_filter |> 
      mutate(beta.sum = kfre_pi(datos_filter))
    
    fit <- CSC(Hist(time5y, eventd5y) ~ offset(beta.sum), 
               data = datos_filter,
               cause = 1)
    
    p1 <- 1 - as.vector(predictRisk(fit, 
                                    newdata = data.frame(beta.sum = 0), 
                                    times = horizon, 
                                    cause = 1))
    
    fc.aj.risk.basal[i] <- p1
  }
  
  return(list(fc.aj.risk.basal.imp.csh = fc.aj.risk.basal, 
              fc.aj.risk.basal.csh = mean(fc.aj.risk.basal)))
}