
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
cat("\014") ##clear console
rm(list=ls()) ## clear workspace

library(ggplot2)
library(latex2exp)
library(dplyr)
library(gridExtra)

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

DLM.plot <- data.frame("Date" = dates,"Obs" = exp(y), "Est" = exp(DLM.sp$f))


#----- print results ----------
dpi.sp <- 300

pp.theme <- theme(axis.ticks.length=unit(0.15, "cm"),
                  axis.text.x = element_text(size=10),
                  axis.text.y = element_text( size=10),
                  axis.title.x =  element_text(size = 12),
                  axis.title.y =  element_text(size = 12),
                  panel.grid.major = element_line(color = "darkgrey", size = 0.3,
                                                  linetype = 1),
                  panel.grid.minor = element_line(color = "darkgrey", size = 0.3,
                                                  linetype = 2)) 

pp.legend <- theme( 
  legend.position = c(0.8, 0.9),
  legend.background=element_rect(fill="transparent",colour=NA),
  legend.text = element_text(size = 11),
  legend.title =  element_blank(),
  legend.key.size = unit(0.5, 'lines'),
  legend.key.width = unit(0.5,"in"))

p <- list()

#---- scatter plot
y.min <- min(DLM.plot$Obs, DLM.plot$Est, na.rm = T)
y.max <- max(DLM.plot$Obs, DLM.plot$Est, na.rm = T)
xylim.sp <- c(y.min, y.max) 
p[[1]] <- ggplot(data = DLM.plot, aes(x = Est, y = Obs)) +
  theme_bw() + 
  theme_light(base_size = 7) +
  geom_point(size = 2, color = "dodgerblue3", pch = 21, stroke = 1) +
  scale_x_log10(limits = xylim.sp) +
  scale_y_log10(limits = xylim.sp) +
  labs(y="Observation", x="Estimation") +
  geom_abline(linetype = 1) +
  annotate("text", 
           x = c(xylim.sp[2]*0.02, xylim.sp[2]*0.02), 
           y = c(xylim.sp[1]*2, xylim.sp[1]),
           label = c(paste0("Model: ", m.lab),
                     paste0("NSE(log): ", round(R2.log,2))),
           hjust = 0, size = c(4)) +
  pp.theme 

#---- time series 
col.line <- c("Observed" = "forestgreen",
               "Estimated"="coral")
xylim.sp <- c(min(DLM.plot[,-1], na.rm = T), max(DLM.plot[,-1], na.rm = T))

p[[2]] <- ggplot(data = DLM.plot) +
  theme_bw() + 
  theme_light(base_size = 7) +
  geom_point(aes(x = Date, y = Obs,  color = paste0("Observed")),
             size = 2, pch = 21, stroke = 1) +
  scale_color_manual(values = col.line) +
  ggnewscale::new_scale_color() +
  geom_line(aes(x = Date, y = Est, color = paste0("Estimated")),
            size = 0.8) +
  scale_color_manual(values = col.line) +
  scale_y_log10(limits = xylim.sp) +
  labs(y="Constituent", x="Date") +
  pp.legend +
  pp.theme 

#---- DLM parameters
DLM.para <- data.frame("Date" = dates, DLM.sp$m)
names(DLM.para)[2:3] <- c("Intercept","Slope")


#---- Intercept
STRP.dates <- as.Date(c("2012-09-30","2013-09-15","2013-10-31","2014-07-31",
                        "2014-09-30","2014-10-01","2015-09-30","2016-09-30"))
SSC.max <- max(DLM.sp$m[,1])

STRP.arrows <- data.frame(
  x = STRP.dates, y = rep(6,8),
  xend = STRP.dates, yend = rep(5,8))

p[[3]] <- ggplot() +
  theme_bw() + 
  theme_light(base_size = 7) +
  geom_line(data = DLM.para, aes(x = Date, y = Intercept), color = "firebrick",
            size = 0.8) +
  scale_y_continuous(limits = c(-2,6)) +
  geom_segment(data = STRP.arrows, aes(x = x, y = y, xend = xend, yend = yend),
               color="black",
               arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", 
           x = DLM.para$Date[1], 
           y = -1.5,
           label = c("Arrows: dates of project installations"),
           hjust = 0, size = c(5)) +
  labs(y="Intercept", x="Date") +
  pp.theme 

print(p[[3]])

#--- Slope
p[[4]] <- ggplot(data = DLM.para) +
  theme_bw() + 
  theme_light(base_size = 7) +
  geom_line(aes(x = Date, y = Slope), color = "firebrick",
            size = 0.8) +
  scale_y_continuous(limits = c(-5,5)) +
  labs(y="Slope", x="Date") +
  pp.theme 

p.all <- grid.arrange(arrangeGrob(p[[1]],p[[2]],p[[3]],p[[4]],
                                  nrow = 2))

filename <- "DLM_results.jpeg"
ggsave(filename, plot = p.all,
       width = 10, height = 8, units = "in", dpi = dpi.sp)
