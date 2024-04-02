performance_measures <- function(data, horizon, primary_event, pred, ...) {
  
  ## OE  ratio
  
  obj <- summary(survfit(Surv(time, eventdf) ~ 1, 
                         data = data), 
                 times = horizon)
  
  avg_obs <- obj$pstate[, primary_event + 1]
  
  se_avg_obs <- obj$std.err[, primary_event + 1]
  
  avg_pred <- mean(data[[pred]], na.rm = TRUE)
  
  oe_ratio <- avg_obs / avg_pred
  
  oe_diff <- avg_obs - avg_pred
  
  ## Calibration Intercept
  
  score_vdata <- Score(
    list("csh_validation" = data[[pred]]),
    formula = Hist(time, eventd) ~ 1,
    cens.model = "km",
    data = data,
    conf.int = FALSE,
    times = horizon,
    summary = c("ipa"),
    cause = primary_event,
    plots = "calibration"
  )
  
  # First compute riskRegression::Score()
  
  pseudos <- tibble(score_vdata$Calibration$plotframe)
  
  # Use pseudo-observations calculated by Score() (can alternatively use pseudo::pseudoci)
  
  
  # Note:
  # - 'pseudos' is the data.frame with ACTUAL pseudo-observations, not the smoothed ones
  # - Column ID is not the id in vdata; it is just a number assigned to each row of
  # the original validation data sorted by time and event indicator
  
  pseudos$cll_pred <- log(-log(1 - pseudos$risk)) # add the cloglog risk ests 
  
  # Fit model for calibration intercept
  fit_cal_int <- geese(
    pseudovalue ~ offset(cll_pred),
    data = pseudos,
    id = ID,
    scale.fix = TRUE,
    family = gaussian,
    mean.link = "cloglog",
    corstr = "independence",
    jack = TRUE
  )
  
  fit_cal_slope <- geese(
    pseudovalue ~ offset(cll_pred) + cll_pred,
    data = pseudos,
    id = ID,
    scale.fix = TRUE,
    family = gaussian,
    mean.link = "cloglog",
    corstr = "independence",
    jack = TRUE
  )
  
  ## AUC y Brier Score
  score_vdata <- Score(
    list("csh_validation" = as.matrix(data[[pred]])),
    formula = Hist(time, eventd) ~ 1,
    cens.model = "km",
    data = data,
    conf.int = TRUE,
    times = horizon,
    metrics = c("auc", "Brier"),
    cause = primary_event,
    plots = "calibration"
  )
  
  return(tibble(
    term = c("Average predicted risk", 
             "Overall observerd risk", 
             "OE ratio", 
             "OE difference", 
             "Calibration Intercept", 
             "Calibration Slope", 
             "AUC", 
             "Brier" 
             ), 
    estimate = c(avg_pred, avg_obs, oe_ratio, oe_diff, 
                 summary(fit_cal_int)$mean$estimate, 
                 summary(fit_cal_slope)$mean["cll_pred", "estimate"], 
                 score_vdata[["AUC"]][["score"]][["AUC"]], 
                 score_vdata[["Brier"]][["score"]][["Brier"]][[2]] 
                 ), 
    se = c(NA, se_avg_obs, NA, NA, 
           summary(fit_cal_int)$mean$san.se, 
           summary(fit_cal_slope)$mean["cll_pred", "san.se"], 
           score_vdata[["Brier"]][["score"]][["se"]][[2]], 
           score_vdata[["AUC"]][["score"]][["se"]] 
           ), 
    var = c(NA, se_avg_obs, NA, NA,
            summary(fit_cal_int)$mean$san.se ^ 2, 
            summary(fit_cal_slope)$mean["cll_pred", "san.se"] ^ 2, 
            score_vdata[["AUC"]][["score"]][["se"]] ^ 2, 
            score_vdata[["Brier"]][["score"]][["se"]][[2]] ^ 2 
            )))
}