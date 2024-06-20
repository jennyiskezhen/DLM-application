
Data_preprocess <- function(df){
  
  Data <- df
  Data$Date <- as.Date(Data$Date)
  
  #---- constraint data to start/end with complete data ----
  Data.0 <- df
  Data.0 <- Data.0[complete.cases(Data.0),]
  
  dates.start <- Data.0$Date[1]
  dates.end <- Data.0$Date[length(Data.0$Date)]
  dates.0 <- seq(as.Date(dates.start),
                 as.Date(dates.end),by="days")
  
  Xt.0 <- Data[Data$Date%in%dates.0,2]
  Yt.0 <- Data[Data$Date%in%dates.0,3]
  #-------------------------------------------------
  
  #---- remove the beginning 0 from log transformation ----
  NXt.0 <- Xt.0/median(Xt.0,na.rm = T)
  Xt.1 <- log(NXt.0)
  # Xt.1 <- log(Xt.0)
  Yt.1 <- log(Yt.0)
  
  x.non0 <- which(Xt.1!=0)[1]
  y.non0 <- which(Xt.1!=0)[1]
  
  if (x.non0 < y.non0){
    Xt.2 <- Xt.1[y.non0:length(Xt.1)]
    Yt.2 <- Yt.1[y.non0:length(Yt.1)]
    dates <- dates.0[y.non0:length(dates.0)]
  } else {
    Xt.2 <- Xt.1[x.non0:length(Xt.1)]
    Yt.2 <- Yt.1[x.non0:length(Yt.1)]
    dates <- dates.0[x.non0:length(dates.0)]
  }
  #----------------------
  
  Xt <- Xt.2
  Yt <- Yt.2
  
  df.final <- data.frame("Date" = dates, 
                         "Xt" = Xt, "Yt" = Yt,
                         "Flow" = Xt.0)
  
  return(df.final)
  
}
