
NSE <- function (x, y) {
  
  df <- data.frame(x,y)
  df <- df[complete.cases(df),]
  
  z <- 1 - sum((df$y-df$x)^2)/sum((df$x-mean(df$x))^2)
  return (z)
  
}