
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
cat("\014") ##clear console
rm(list=ls()) ## clear workspace

source("Data_preprocess.R")
source("DLM_example.R")
source("NSE.R")

############# specification #############
### Data format: Date, Flow, Constituent ###
sq.sp <- 1 # 1) linear structure; 2) quadratic structure
######################

if (sq.sp == 1){
  m.lab <- "Linear formulation" 
} else if (sq.sp == 2){
  m.lab <- "Quadratic formulation"
}

Data <- read.csv("data/Data_use.csv")

#--------- preprocess data -----
df <- Data_preprocess(Data)
#------------------------------
dates <- df$Date
x <- df$Xt 
y <- df$Yt 
flow <- df$Flow

#----------- DLM hyperparameter -----
delta0 <- 0.99
delta1 <- 0.999999
#-----------------------------

#----- run DLM model -----
DLM.sp <- DLM(X=x, Y=y, delta1=delta0,delta2=delta1, sq = sq.sp)

R2.log <- NSE(y, DLM.sp$f)
R2 <- NSE(exp(y), exp(DLM.sp$f))
#------------------------

#----- print results ----------
filename <- paste0("DLM_results.jpeg")
jpeg(filename, width = 10, height = 7, units = "in", res = 300)

par(mfrow=c(2,2),mar=c(2.5,2.5,1,1), mgp=c(1.5,0.5,0))
par(cex.lab = 1, cex.sub = 1, cex.axis = 1, cex = 1, font = 1) 
options(scipen=10)

y.min <- min(exp(y),exp(DLM.sp$f), na.rm = T)
y.max <- max(exp(y),exp(DLM.sp$f), na.rm = T)

plot(exp(DLM.sp$f), exp(y), log = "xy",
     ylim = c(y.min, y.max),
     xlim = c(y.min, y.max),
     xlab = "Est", ylab = "Obs")
grid(NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = FALSE)

legend("bottomright",
       legend = c(
         paste0("Model: ", m.lab),
         paste0("NSE(log): ", round(R2.log,2))
       ), inset=c(0,0), 
       cex = 0.8,
       bty = "n")

plot(dates, exp(y),ylim = c(y.min, y.max),
     log = "y",col = "forestgreen",
     xlab = "Date", ylab = "Water quality")
lines(dates, exp(DLM.sp$f), col = "firebrick")
grid(NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = FALSE)
legend("topright",
       legend = c(
         "Observation",
         "Estimation"
       ),
       lty = c(NA,1),
       pch = c(1, NA),
       col = c("forestgreen", "firebrick"),
       cex = 0.8,
       bty = "n")

plot(df$Date, DLM.sp$m[,1],
     ylim = c(-2,6),  type = "l",
     xlab = "Date", ylab="Intercept")
grid(NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = FALSE)

plot(df$Date, DLM.sp$m[,2],
     ylim = c(-5,5),  type = "l",
     xlab= "Date", ylab="Slope")
grid(NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = FALSE)

dev.off()
