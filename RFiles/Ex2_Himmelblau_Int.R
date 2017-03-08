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

postscript('Fig/fig2-ex2-plot.eps', w = 14, h = 7, paper = "special", horizontal = FALSE)
par(mfcol = c(1,2))
plot(int_programming)
plot(int_programming, type = "contour", lower = c(-5, -5), upper = c(5, 5))
dev.off()
