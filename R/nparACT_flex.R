nparACT_flex <-
function (name, SR, cutoff = 1, minutes, plot = T){
  data <- get(name)
  if (is.data.frame(data)==F){
    data = as.data.frame(data)
  }
  if(is.numeric(data[1]) == F){
    data[,1] <- as.POSIXct(data[,1], format="%H:%M:%S")
    names(data)[1] <- "time"
    names(data)[2] <- "activity"
  } else {
    data[,2] <- as.POSIXct(data[,2], format="%H:%M:%S")
    names(data)[2] <- "time"
    names(data)[1] <- "activity"
  }
  bin_hr <- 60   
  a <- nrow(data) 
  b <- floor(a/(SR*60)) 
  e <- SR*60 
  m <- bin_hr*SR*60  
  
  ## ---- Filtering, Cutoff for classification as movement
  nparACT_auxfunctions1$nparACT_filt(data, a, cutoff)
  ## ------------------------------------------
  
  ## ---- Calculate average for each minute (needed if SR != 1)
  if (SR != 1){
    data_min <- nparACT_auxfunctions1$nparACT_data_min(b, SR, data)
  } else {
    data_min <- data
  }
  ## ------------------------------------------
  
  ## ---- Calculate hourly averages
  data_hrs <- nparACT_auxfunctions1$nparACT_data_hrs(data, a, m)
  ## -----------------------------------------------------------------------------
  ## -----------------------------------------------------------------------------
  
  ## ---- Plot hourly data
  if (plot == T) {
    nparACT_auxfunctions2$nparACT_plot_hourly(data, data_hrs, SR)
  }
  
  ## ---- IS/IV calculation (based on data_hrs!)
  result_ISIV <- nparACT_ISIVfunctions$nparACT_ISIV(data_hrs, bin_hr)
  IS <- result_ISIV[1]
  IV <- result_ISIV[2]
  ## ---------------------------------------------------------------------------------
  
  ## ---------- Relative Amplitude (RA) calculation
  ## ---- Minutewise averages across 24hrs
  minaverage <- nparACT_auxfunctions1$nparACT_minaverage(a, data_min)
  ## --------------------------------
  
  ## ---- Plot Minutewise averages
  if (plot == T){
    start.time = NULL
    nparACT_auxfunctions2$nparACT_plot_minaverage(data, minaverage, start.time, a, SR)
  }
  ## --------------------------------      
  
  ## ---- Plot Hourly averages
  if (plot == T){
    nparACT_auxfunctions2$nparACT_plot_hraverage(data, minaverage, start.time, a, SR)
  }
  ## --------------------------------      
  
  ## ---- L5, M10, Lflex values
  result_RA <- nparACT_RAfunctions$nparACT_L5M10Lflex(data, minaverage, a, SR, minutes)
  L5 <- result_RA[1]
  L5_starttime <- result_RA[2]
  M10 <- result_RA[3]
  M10_starttime <- result_RA[4]
  Lflex <- result_RA[5]
  Lflex_starttime <- result_RA[6]
  RA <- result_RA[7]

  nparACT_result <- data.frame(IS, IV, RA, L5, L5_starttime, M10, M10_starttime, Lflex, Lflex_starttime)
  return (nparACT_result)
}
