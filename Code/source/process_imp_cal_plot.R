process_imp_cal_plot <- function(i, vdata, primary_event, horizon, type, n_internal_knots = 5) {
  
  if (type == "pseudoval_loess") {
    
    if (i == 0) {
      
      vdata_filt <- vdata |>  
        filter(.imp == 0) |> 
        drop_na(pred, time, eventd)
      
      vdata_elim <- vdata |> 
        filter(.imp == 0) |> 
        select(.imp, .id, eventd) |> 
        anti_join(vdata_filt, by = ".id") |> 
        mutate(ID = .id, 
               times = horizon, 
               pseudovalue = as.numeric(NA), 
               time = as.numeric(NA), 
               status = case_match(eventd, 
                                   c(1, 2) ~ 1, 
                                   0 ~ 0), 
               event = case_match(eventd, 
                                  1 ~ 1, 
                                  2 ~ 2, 
                                  0 ~ 3), 
               WTi = as.numeric(NA), 
               model = "csh_validation", 
               risk = as.numeric(NA), 
               Wt = as.numeric(NA)) |> 
        select(-eventd) |> 
        select(.imp, .id, ID, times, pseudovalue, time, status, event, WTi, model, 
               risk, Wt)
      
    } else {
      vdata_filt <- vdata %>% 
        filter(.imp == i) 
    }
    
    pred <- as.matrix(vdata_filt$pred)
    
    score_vdata <- Score(
      list("csh_validation" = pred),
      formula = Hist(time, eventd) ~ 1,
      cens.model = "km",
      data = vdata_filt,
      conf.int = TRUE,
      times = horizon,
      summary = c("ipa"),
      cause = primary_event,
      plots = "calibration"
    )
    
    datos <- data.frame(score_vdata$Calibration$plotframe) |> 
      mutate(.id = vdata_filt$.id, .imp = as.integer(i)) |> 
      select(.imp, .id, everything()) 
    
    if (i == 0) {
      datos <- datos |> 
        mutate(ID = .id)
      
      datos <- datos |> 
        bind_rows(vdata_elim) |> 
        arrange(.imp, .id)
      
      return(datos)
      
    } else {
      return(datos)
    }
  } else if (type == "subdist_hazard") {
    
    if (i == 0) {
      
      vdata_filt <- vdata |>  
        filter(.imp == 0) |> 
        drop_na(pred, time, eventd) 
      
      vdata_elim <- vdata |> 
        filter(.imp == 0) |> 
        select(.imp, .id) |> 
        anti_join(vdata_filt, by = ".id") |> 
        mutate(obs = as.numeric(NA), 
               risk = as.numeric(NA)) |> 
        select(.imp, .id, obs, risk)
      
    } else {
      vdata_filt <- vdata %>% 
        filter(.imp == i) 
    }
    
    # 5 knots seems to give somewhat equivalent graph to pseudo method with bw = 0.05
    rcs_vdata <- ns(vdata_filt$cll_pred, df = n_internal_knots + 1)
    colnames(rcs_vdata) <- paste0("basisf_", colnames(rcs_vdata))
    # vdata_bis <- cbind.data.frame(vdata_filt, rcs_vdata)
    vdata_bis <- vdata_filt |> 
      bind_cols(as.data.frame(rcs_vdata))
    
    # Use subdistribution hazards (Fine-Gray) model
    form_fgr <- reformulate(
      termlabels = colnames(rcs_vdata),
      response = "Hist(time, eventd)"
    )
    
    # Regress subdistribution of event of interest on cloglog of estimated risks
    calib_fgr <- FGR(
      formula = form_fgr,
      cause = primary_event,
      data = vdata_bis, 
      variance = FALSE
    )
    
    obs <- predict(calib_fgr, times = horizon, newdata = vdata_bis)
    
    # Add observed and predicted together in a data frame
    datos <- data.frame(
      .imp = vdata_bis$.imp,
      .id = vdata_bis$.id,
      "obs" = obs[, 1],
      "risk" = vdata_bis$pred
    )
    
    if (i == 0) {
      datos <- datos |> 
        bind_rows(vdata_elim) |> 
        arrange(.imp, .id)
      
      return(datos)
      
    } else {
      return(datos)
    }
    
  } else {
    print("Error: Method not implemented")
  }
  
}