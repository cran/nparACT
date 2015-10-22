nparACT_flex_loop <-
function (path, SR, cutoff=1, minutes, plot = T){
  files <- list.files(path)
  nofiles <- length(files)
  bin_hr <- 60  
  nparACT_result <- matrix(NA, nofiles, 9)
  nparACT_result <- as.data.frame(nparACT_result)
  colnames(nparACT_result) <- c("IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime", "Lflex", "Lflex_starttime")
  matrix_hraverage <- matrix(NA, nofiles, 24)
  for (zz in 1:nofiles){
    name <- files[zz]
    data <- read.table(paste(path,name, sep="/"), header = F)
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
    }  else {
      data_min <- data
    }
    ## ------------------------------------------
    
    ## ---- Calculate hourly averages
    data_hrs <- nparACT_auxfunctions1$nparACT_data_hrs(data, a, m)
    ## -----------------------------------------------------------------------------
    
    ## ---- IS/IV calculation (based on data_hrs!)
    result_ISIV <- nparACT_ISIVfunctions$nparACT_ISIV(data_hrs, bin_hr)
    IS <- result_ISIV[1]
    IV <- result_ISIV[2]
    nparACT_result[zz,1] <- IS
    nparACT_result[zz,2] <- IV
    ## ---------------------------------------------------------------------------------
    
    ## ---------- Relative Amplitude (RA) calculation
    ## ---- Minutewise averages across 24hrs
    minaverage <- nparACT_auxfunctions1$nparACT_minaverage(a, data_min)
    ## --------------------------------
    
    ## ----- Compute & Plot hourly averages (Grand Average Plot)
    if(plot == T){
      hraverage_sorted <- nparACT_auxfunctions1$nparACT_hraverage_GA_loop(minaverage, data, a , SR)
      matrix_hraverage[zz,] <- hraverage_sorted
    }
    ## --------------------------------------------------
    
    ## ---- L5, M10, Lflex values
    result_RA <- nparACT_RAfunctions$nparACT_L5M10Lflex(data, minaverage, a, SR, minutes)
    result_RA <- as.data.frame(result_RA) 
    
    ## ---- Write results to results matrix
    nparACT_result[zz,3] <- result_RA$RA
    nparACT_result[zz,4] <- result_RA$L5
    nparACT_result[zz,5] <- as.character(result_RA$L5_starttime)
    nparACT_result[zz,6] <- result_RA$M10
    nparACT_result[zz,7] <- as.character(result_RA$M10_starttime)
    nparACT_result[zz,8] <- result_RA$Lflex
    nparACT_result[zz,9] <- as.character(result_RA$Lflex_starttime)
    ## ------------------------------------------------------------------------------
    ## ------------------------------------------------------------------------------
  }
  nparACT_result <- nparACT_result
  if (plot == T){
    nparACT_auxfunctions2$nparACT_plot_hraverage_GA_loop(matrix_hraverage)
  }
  return (nparACT_result)
}