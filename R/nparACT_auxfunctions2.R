nparACT_auxfunctions2 <- list(
  nparACT_plot_hourly = function(data, data_hrs, SR){
    a <- nrow(data) 
    hours <- nrow(data_hrs)
    days <- ceiling(hours/24)
    days_hours <- days*24
    
    daytime <- matrix(NA)
    time <- data$time
    time <- as.character(time)
    for (v in seq(1,a,(SR*60*60))){  
      daytime[v] <- time[v]
    }
    daytime <- na.omit(daytime)
    daytime <- as.character(daytime)
    
    temp = unlist(str_split(daytime, ' ') )
    temp_nums = 1:length(temp)
    timeinfo = temp[ (temp_nums %% 2) == 0 ] 
    temp = unlist(str_split(timeinfo, ':') )
    temp_nums = 1:length(temp)
    timeinfo = temp[ (temp_nums %% 3) == 1 ] 
    start.time <- as.numeric(timeinfo[1])  
    
    mf_labeller <- function(variable, value){
      if (variable == "day_count"){
        value[value == 1] <- "Days 1-2"
        value[value == 2] <- "Days 3-4"
        value[value == 3] <- "Days 5-6"
        value[value == 4] <- "Days 7-8"
        value[value == 5] <- "Days 9-10"
        value[value == 6] <- "Days 11-12"
        value[value == 7] <- "Days 13-14"
        value[value == 8] <- "Days 15-16"
        value[value == 9] <- "Days 17-18"
        value[value == 10] <- "Days 19-20"
        value[value == 11] <- "Days 21-22"
        value[value == 12] <- "Days 23-24"
        value[value == 13] <- "Days 25-26"
        value[value == 14] <- "Days 27-28"
        value[value == 15] <- "Days 29-30"
      }
      return(value)
    }
    
    fill <- rep(NA, start.time-1)
    
    data_hrs2 <- data_hrs
    data_hrs2 <- c(fill, data_hrs2)
    hours_plot <- length(data_hrs2)
    days_plot <- ceiling(hours_plot/24)
    days_hours_plot <- days_plot*24
    data_hrs2[days_hours_plot] <- NA 
    
    hours_count <- seq(1:24)
    hours_count <- rep(hours_count,days_plot)
    
    count_plot <- seq(1:48) 
    count_plot <- rep(count_plot, length.out = days_hours_plot)
    
    day_count <- NA 
    for (t in 1:(ceiling(days_plot/2))){
      day_count[((t-1)*48+1):(t*24*2)] <- rep(t,24)
    }
    day_count <- day_count[1:days_hours_plot]
    
    df_plot <- data.frame(day_count, count_plot, data_hrs2)
    
    p <- ggplot(df_plot, aes(x=count_plot, y = data_hrs2))+ 
      geom_bar(stat="identity", width = 1, position = position_dodge(width = 0.5))+
      theme_bw()+
      facet_grid(day_count ~ ., labeller = mf_labeller)+
      scale_x_discrete(limits = c(seq(1:48)), breaks = seq(2,48,2))+
      xlab("Time \n (Start: 0am)")+
      ylab("Movement Intensity")+
      ggtitle("Actigraphy Plot (48 hrs)\n Dual Day Display")+
      theme(plot.margin=unit(c(1,14,1,14),"cm"),
            axis.text.x = element_blank(),
            axis.title.x = element_text(face="bold", size=14),
            axis.title.y = element_text(face="bold", size=14, vjust = 1.2),
            plot.title = element_text(face="bold", size=16, vjust = 1))
    print(p)
  },
  
  nparACT_plot_hraverage = function(data, minaverage, start.time, a, SR){
    hraverage <- matrix(NA)
    for (i in 1:24){
      hraverage [i] <- mean(minaverage[(((i-1)*60)+1):(60*i)])
    }
    daytime <- matrix(NA)
    time <- data$time
    time <- as.character(time)
    for (v in seq(1,a,(SR*60*60))){  
      daytime[v] <- time[v]
    }
    daytime <- na.omit(daytime)
    daytime <- as.character(daytime)
    temp = unlist(str_split(daytime, ' ') )
    temp_nums = 1:length(temp)
    timeinfo = temp[ (temp_nums %% 2) == 0 ] 
    temp = unlist(str_split(timeinfo, ':') )
    temp_nums = 1:length(temp)
    timeinfo = temp[ (temp_nums %% 3) == 1 ] 
    start.time <- as.numeric(timeinfo[1])  
    
    for_hraverage_plot.time <- rep(seq(1,24),2)
    seq <- seq(start.time, length.out = 24)
    hraverage_plot_time <- for_hraverage_plot.time[seq]
    hraverage_plot_df <- data.frame (hraverage_plot_time, hraverage)
    ppp <- ggplot(hraverage_plot_df, aes(x=hraverage_plot_time, y = hraverage))+ 
      geom_bar(stat="identity", width = 1, position = position_dodge(width = 0.5))+
      theme_bw()+
      scale_x_discrete(limits = c(seq(1:24)), breaks = seq(1,24))+
      xlab("Time \n (Start: 0am)")+
      ylab("Movement Intensity")+
      ggtitle("Actigraphy Plot (24 hrs)\n Average across days")+
      theme(plot.margin=unit(c(1,2,1,2),"cm"),
            axis.text.x = element_blank(),
            axis.title.x = element_text(face="bold", size=14),
            axis.title.y = element_text(face="bold", size=14, vjust = 1.2),
            plot.title = element_text(face="bold", size=16, vjust = 1))
    print(ppp)
  },
  
  nparACT_plot_minaverage = function(data, minaverage, start.time, a, SR){
    daytime <- matrix(NA)
    time <- data$time
    time <- as.character(time)
    for (v in seq(1,a,(SR*60*60))){  
      daytime[v] <- time[v]
    }
    daytime <- na.omit(daytime)
    daytime <- as.character(daytime)
    
    temp = unlist(str_split(daytime, ' ') )
    temp_nums = 1:length(temp)
    timeinfo = temp[ (temp_nums %% 2) == 0 ] 
    temp = unlist(str_split(timeinfo, ':') )
    temp_nums = 1:length(temp)
    timeinfo = temp[ (temp_nums %% 3) == 1 ] 
    start.time <- as.numeric(timeinfo[1]) 
    
    for_minaverage_plot.time <- rep(seq(1,1440),2)
    seq <- seq(start.time*60, length.out = 1440)
    minaverage_plot_time <- for_minaverage_plot.time[seq]
    
    minaverage_plot_df <- data.frame (minaverage_plot_time, minaverage)
    
    pp <- ggplot(minaverage_plot_df, aes(x=minaverage_plot_time, y = minaverage))+ 
      geom_bar(stat="identity", width = 1, position = position_dodge(width = 0.5))+
      theme_bw()+
      scale_x_discrete(limits = c(seq(1:1440)), breaks = seq(1,1440,60))+
      xlab("Time \n (Start: 0am)")+
      ylab("Movement Intensity")+
      ggtitle("Actigraphy Plot (24 hrs)\n Average across days")+
      theme(plot.margin=unit(c(1,2,1,2),"cm"),
            axis.text.x = element_blank(),
            axis.title.x = element_text(face="bold", size=14),
            axis.title.y = element_text(face="bold", size=14, vjust = 1.2),
            plot.title = element_text(face="bold", size=16, vjust = 1))
    print(pp)
  },
  
  nparACT_plot_hraverage_GA_loop = function(matrix_hraverage){
    GA_hraverage <- colMeans(matrix_hraverage)
    GA_hraverage_plot_time <- seq(1,24)
    
    GA_hraverage_plot_df <- data.frame (GA_hraverage_plot_time, GA_hraverage)
    
    p <- ggplot(GA_hraverage_plot_df, aes(x=GA_hraverage_plot_time, y = GA_hraverage))+ 
      geom_bar(stat="identity", width = 1, position = position_dodge(width = 0.5))+
      theme_bw()+
      scale_x_discrete(limits = c(seq(1:24)), breaks = seq(1,24))+
      xlab("Time \n (Start: 0am)")+
      ylab("Movement Intensity")+
      ggtitle("Grand Average Actigraphy Plot (24 hrs)\n Average across days")+
      theme(plot.margin=unit(c(1,2,1,2),"cm"),
            axis.text.x = element_blank(),
            axis.title.x = element_text(face="bold", size=14),
            axis.title.y = element_text(face="bold", size=14, vjust = 1.2),
            plot.title = element_text(face="bold", size=16, vjust = 1))
    print(p)
  }
)