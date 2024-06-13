#title: "Does Climate Impact the Relationship Between the Energy Price and the Stock Market? The Colombian Case"
#analysis: "Wavelets"

rm(list=ls())

library(readxl)
library(WaveletComp)

Datos <- read_excel("Datos.xlsx")
attach(Datos)


###### INDIVIDUAL SERIES #######

my.data <- data.frame(x = PBN)
my.data$date <- seq(as.Date("2001/7/1"), as.Date("2021/12/31"), by ="day")
rownames(my.data) <- my.data$date

my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 32,
                        upperPeriod = 1024,
                        make.pval = FALSE, n.sim = 100)

wt.image(my.w, color.key = "quantile", n.levels = 100,show.date  =  TRUE,
         periodlab="period (days)" ,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))

reconstruct(my.w, plot.waves = FALSE, lwd = c(1,1), show.date = TRUE)

my.data <- data.frame(x = IGBC)
my.data$date <- seq(as.Date("2001/7/1"), as.Date("2021/12/31"), by ="day")
rownames(my.data) <- my.data$date
my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 32,
                        upperPeriod = 1024,
                        make.pval = FALSE, n.sim = 100)

wt.image(my.w, color.key = "quantile", n.levels = 100,show.date  =  TRUE, 
         periodlab="period (days)" , 
         legend.params = list(lab = "wavelet power levels", mar = 4.7))

reconstruct(my.w, plot.waves = FALSE, lwd = c(1,1), show.date = TRUE)


####### WAVELETS COHERENCE   #####

my.data <- data.frame(x = PBN, y = IGBC)
my.data$date <- seq(as.Date("2001/7/1"), as.Date("2021/12/31"), by ="day")
rownames(my.data) <- my.data$date
my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0,
                           dt = 1, dj = 1/250,
                           lowerPeriod = 32,
                           upperPeriod = 1024,
                           make.pval = TRUE, n.sim = 100)

wc.image(my.wc, n.levels = 100,show.date  =  TRUE, main="Cross-wavelet power spectrum of PBN and IGBC",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "", periodlab = "period (Days)")

wc.avg(my.wc, siglvl = 0.01, sigcol = "red", sigpch = 20,
       periodlab = "period (Days)", legend.coords = "topleft")


