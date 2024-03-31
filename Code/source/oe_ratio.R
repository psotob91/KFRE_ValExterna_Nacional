# Razon O/E
oe_ratio <- function(data, horizon = 5, primary_event = 1, pred) {
  obj <- summary(survfit(Surv(time5y, eventd5ylab) ~ 1, data = data), times = horizon)
  avg_obs <- obj$pstate[, primary_event + 1]
  se_avg_obs <- obj$std.err[, primary_event + 1]
  avg_pred <- mean(pred, na.rm = TRUE)
  oe_ratio <- avg_obs / avg_pred
  return(data.frame(estimate = oe_ratio, 
                    se = se_avg_obs, 
                    var = se_avg_obs ^ 2, 
                    avg_obs = avg_obs,
                    avg_pred = avg_pred
  ))
}