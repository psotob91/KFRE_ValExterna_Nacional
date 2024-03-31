# Funcion que calcula indice pronostico de recal.risk.coef
# (de recalibracion de coeficientes usando Cox)
kfre_pi.coef <- function(data) {
  recal.risk.coef(data, 5)$fc.aj.coef * (-0.2201 * (data$age / 10 - 7.036) + 0.2467 * (as.integer(data$sex == "Masculino") - 0.5642) - 0.5567 * (data$eGFR_ckdepi / 5 - 7.222) + 0.4510 * (log(data$acr) - 5.137))
}
