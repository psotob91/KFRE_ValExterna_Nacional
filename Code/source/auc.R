# AUC
auc <- function(data, horizon, primary_event, pred) {
  score_vdata <- Score(
    list("csh_validation" = as.matrix(pred)),
    formula = Hist(time, eventd) ~ 1,
    cens.model = "km",
    data = data,
    conf.int = TRUE,
    times = horizon,
    metrics = c("auc"),
    cause = primary_event,
    plots = "calibration"
  )
  
  return(data.frame(estimate = score_vdata[["AUC"]][["score"]][["AUC"]], 
                    se = score_vdata[["AUC"]][["score"]][["se"]], 
                    var = score_vdata[["AUC"]][["score"]][["se"]] ^ 2))
}