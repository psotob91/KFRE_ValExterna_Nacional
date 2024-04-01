# Fuente: https://stats.stackexchange.com/questions/63600/how-to-translate-the-results-from-lm-to-an-equation

## Opcion 1
library(dplyr)

model_equation <- function(model, ...) {
  format_args <- list(...)
  
  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}
library(MASS)
modelcrime <- lm(y ~ ., data = UScrime)
model_equation(modelcrime, digits = 3, trim = TRUE)

## Opcion 2
library(MASS)
crime.lm <- lm(y~., UScrime)
cc <- crime.lm$coefficients
(eqn <- paste("Y =", paste(round(cc[1],2), paste(round(cc[-1],2), names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e"))
# "Y = -5984.29 + 8.78 * M + -3.8 * So + 18.83 * Ed + 19.28 * Po1 + -10.94 * Po2 + -0.66 * LF + 1.74 * M.F + -0.73 * Pop + 0.42 * NW + -5.83 * U1 + 16.78 * U2 + 0.96 * GDP + 7.07 * Ineq + -4855.27 * Prob + -3.48 * Time + e"

(eqn <- gsub('\\+ -', '- ', gsub(' \\* ', '*', eqn)))

## Opcion 3
library(equatiomatic)
### Ver foro