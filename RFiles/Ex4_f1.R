#--------------------------------------------------#
#### Shifted and Rotated Expanded Two-Peak Trap ####
#--------------------------------------------------#
# From Qu et al.: Novel Benchmark Functions for Continuous Multimodal Optimization with Comparative Results

## Clear workspace ##
rm(list = ls())

## Load packages ##
library(GenSA)
library(optimization)
library(xtable)
library(microbenchmark)

## setwd ##
setwd('/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/')

## xpanded Two-Peak Trap function ##
# Global minimun at rep(0, D)
etpt <- function (x) {
  D <- length (x)
  y <- x + 20
  t <- rep(NA, D)
  t[y < 0]           <- -160 + y[y < 0]^2
  t[y >=0 & y < 15] <- (160 / 15) * (y[y >=0 & y < 15] - 15)
  t[y >=15 & y <= 20] <- (200 / 5) * ( 15 - y[y >=15 & y <= 20])
  t[y > 20] <- -200 + (y[y > 20]^2)
  return(sum(t) + 200 * D)
}

tt <- optim_sa(fun = etpt,
         start = (c(25, -25)),
         trace = TRUE,
         lower = c(-50, -50),
         upper=c(50, 50),
         control = list(t0 = 100,
                        t_min = 0.1,
                        nlimit = 500,
                        r = 0.9,
                        rf = 3,
                        ac_acc = 0.1,
                        dyn_rf = TRUE
         )
)
plot(tt, type = 'contour')
tt$function_value
tt$par
tt


optim(par = rep(50, 2), fn = etpt, method = "SANN")

