
source("DLM_example.R")
source("NSE.R")

DLM_cali <- function(df, train.sp, sq){
  
  N <- dim(df)[1]
  
  if (train.sp < 1){
    N.train <- round(N*train.sp)
  } else {
    N.train <- train.sp*365
  }

  df <- df[1:N.train,]
  
  Xt <- df$Xt
  Yt <- df$Yt
  
  #------------ calibration ----
  delta.1 <- c(seq(0.98, 0.999, by=0.002),0.999,0.9995,0.9999)
  delta.0 <- seq(0.7, 0.99, by=0.01)
  NSE.log.v <- array(NA, c(length(delta.0),length(delta.1)))
  
  for (j in 1:length(delta.1)){
    for (i in 1:length(delta.0)){
      
      DLM.Data <- DLM(X=Xt, Y=Yt, delta1=delta.0[i], delta2=delta.1[j], sq)
      
      #--------- calibrate using sum of sq error
      ee.orig <- DLM.Data$e.orig
      ee.log <- DLM.Data$e
      
      df.orig <- data.frame("x" = exp(Yt), "y" = ee.orig)
      df.orig <- df.orig[complete.cases(df.orig),]
      sse.orig <- sum(df.orig$y^2)
      
      e.orig <- sse.orig/sum((df.orig$x-mean(df.orig$x))^2)
      
      df.log <- data.frame("x" = Yt, "y" = ee.log)
      df.log <- df.log[complete.cases(df.log),]
      sse.log <- sum(df.log$y^2)
      
      e.log <- sse.log/sum((df.log$x-mean(df.log$x))^2)
      
      NSE.log.v[i,j] <- e.orig + e.log
    }
  }
  
  i.min <- apply(NSE.log.v, 1, FUN = min)
  j.min <- apply(NSE.log.v, 2, FUN = min)
  
  i.opt <- which.min(i.min)
  j.opt <- which.min(j.min)
  
  df.delta <- data.frame("Delta0" = delta.0[i.opt],
                         "Delta1" = delta.1[j.opt])
  
  return(df.delta)
}