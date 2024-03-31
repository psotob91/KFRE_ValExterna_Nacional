# Funcion que recalibra coeficientes usando Competing Risk Model
recal.risk.coef.csh <- function(data, horizon) {
  
  fc.aj.risk.basal <- rep(NA, max(data$.imp))
  fc.aj.coef <- rep(NA, max(data$.imp))
  
  for (i in 1:max(data$.imp)) {
    
    datos_filter <- data |> 
      filter(.imp == i) 
    
    datos_filter <- datos_filter |> 
      mutate(beta.sum = kfre_pi(datos_filter))
    
    # Calculo del factor de ajuste a los coeficientes
    fit <- CSC(Hist(time5y, eventd5y) ~ beta.sum, 
               data = datos_filter,
               cause = 1)
    
    datos_filter <- datos_filter |> 
      mutate(beta.sum2 = fit[["models"]][["Cause 1"]][["coefficients"]][["beta.sum"]] * beta.sum)
    
    # Recalculo de la supervivencia basal
    fit2 <- CSC(Hist(time5y, eventd5y) ~ offset(beta.sum2), 
                data = datos_filter,
                cause = 1)
    
    p2 <- 1 - as.vector(predictRisk(fit2, 
                                    newdata = data.frame(beta.sum2 = 0), 
                                    times = horizon, 
                                    cause = 1))   
    
    
    fc.aj.risk.basal[i] <- p2
    fc.aj.coef[i] <- fit[["models"]][["Cause 1"]][["coefficients"]][["beta.sum"]]
  }
  
  return(list(fc.aj.risk.basal.imp.csh = fc.aj.risk.basal, 
              fc.aj.risk.basal.csh = mean(fc.aj.risk.basal), 
              fc.aj.coef.imp.csh = fc.aj.coef, 
              fc.aj.coef.csh = mean(fc.aj.coef)))
}