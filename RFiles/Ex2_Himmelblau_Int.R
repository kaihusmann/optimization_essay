#------------------------------------------------------#
#### Example 1: Himmelblau, integer parameter space ####
#------------------------------------------------------#
## Clear workspace ##
rm(list = ls())

## Load packages ##
library(optimization)
library(xtable)
library(microbenchmark)

## setwd ##
setwd('/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/')

## Himmelblau's function ##
# 4 minima at
# f(3, 2) = 0
# f(-2.804, -3.131) = 0
# f(-3.779, -3.283) = 0
# f( 3.584, -1.848) = 0

hi <- function(x){
  (x[1]**2 + x[2] - 11)**2 + (x[1] + x[2]**2 -7)**2
}

var_func_int <- function (para_0, fun_length, rf, temp = NA){
  ret_var_func <- para_0 + sample.int(rf, fun_length, replace = TRUE) *
    ((rbinom(fun_length, 1, 0.5) * -2) + 1)
  return (ret_var_func)
}

var_func_int_gr <- function (x) {
  x1 <- x[1]
  x2 <- x[2]
  c(x1 + sample.int(10, 1, replace = TRUE) * ((rbinom(length(x1), 15, 0.5) * -2) + 1),
  x2 + sample.int(10, 1, replace = TRUE) * ((rbinom(length(x2), 15, 0.5) * -2) + 1))
}

#-----------------------#
## Integer programming ##
#-----------------------#

## optim_sa ##
int_programming <- optim_sa(fun = hi,
         start = (c(10, 10)),
         trace = TRUE,
         lower = c(-40, -40),
         upper=c(40, 40),
         control = list(t0 = 500,
                        nlimit = 50,
                        r = 0.85,
                        rf = 3,
                        ac_acc = 0.1,
                        dyn_rf = TRUE,
                        vf = var_func_int
                        )
         )

own.cex <- 0.6

postscript('Fig/fig2-ex2-plot.eps', w = 14 * own.cex, h = 7 * own.cex, paper = "special", horizontal = FALSE)
par(mfcol = c(1,2))
plot(int_programming)
plot(int_programming, type = "contour", lower = c(-5, -5), upper = c(5, 5))
par(mar=c(3.5, 3.5, 1, 1) +0.1, new = TRUE)
arrows(x0 = -3.5, y0 = 2.5, x1 = -2, y1  = 3, col = 'white', lwd = 4)
arrows(x0 = -3.5, y0 = 2.5, x1 = -2, y1  = 3, col = 'black', lwd = 2)
text(x = -3.9, y = 2.5, "12", cex = 1.2, col = 'white')
text(x = -3.9, y = 2.5, "12", cex = 1, col = 'black')

arrows(x0 = -2.5, y0 = -1.5, x1 = -4, y1  = -1, col = 'white', lwd = 4)
arrows(x0 = -2.5, y0 = -1.5, x1 = -4, y1  = -1, col = 'black', lwd = 2)
text(x = -2.1, y = -1.5, "21", cex = 1.2, col = 'white')
text(x = -2.1, y = -1.5, "21", cex = 1, col = 'black')

dev.off()

# save.image(file = '/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/RFiles/Ex2_Integer.RData')
# load(file = '/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/RFiles/Ex2_Integer.RData')

## optim (SA) ##
optim(par = c(10, 10), fn = hi, gr = var_func_int_gr, method = "SANN", control = list(trace = TRUE))
