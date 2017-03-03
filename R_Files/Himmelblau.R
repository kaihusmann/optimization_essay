#-------------------------------------------------#
#### Example Himmelblau, cont. parameter space ####
#-------------------------------------------------#

library(GanSA)
library(optimization)
library(ggplot2)
# Himmelblau's function
# 4 minima at
# f(3, 2) = 0
# f(-2.804, -3.131) = 0
# f(-3.779, -3.283) = 0
# f( 3.584, -1.848) = 0

hi <- function(x){
  (x[1]**2 + x[2] - 11)**2 + (x[1] + x[2]**2 -7)**2
}

## Find good parameters ##
nloop <- 100
trace.1 <- data.frame(fun = rep(NA, nloop), x1 = rep(NA, nloop), x2 = rep(NA, nloop))
for(i in c(1 : nloop)) {
  trace.1[i, ] <- unlist(optim_sa(fun = hi,
           start = (c(10, 10)),
           trace = TRUE,
           lower = c(-40, -40),
           upper=c(40, 40),
           control = list(t0 = 500,
                          nlimit = 150,
                          r = 0.8,
                          rf = 3,
                          ac_acc = 0.1,
                          dyn_rf = TRUE
           )
  )[c("function_value", "par")])
}
mean(trace.1$fun)
trace.1 <- round(trace.1, digits = 1)
table((apply(trace.1[c(2,3)], 1, paste, collapse = "/")))

## Call optim() with similar parameters
trace.2 <- data.frame(fun = rep(NA, nloop), x1 = rep(NA, nloop), x2 = rep(NA, nloop))
for(i in c(1 : nloop)) {
  trace.2[i, ] <- unlist(optim(par = c(10, 10), fn = hi, method = "SANN", control = list(tmax = 500, reltol = 0.1, temp = 50, trace = TRUE))[c("value", "par")])
}
mean(trace.2$fun)
trace.2 <- round(trace.2, digits = 1)
table((apply(trace.2[c(2,3)], 1, paste, collapse = "/")))

## Call GenSA with similar parametes
trace.3 <- data.frame(fun = rep(NA, nloop), x1 = rep(NA, nloop), x2 = rep(NA, nloop))
for(i in c(1 : nloop)) {
  trace.3[i, ]  <- unlist(GenSA::GenSA(par = c(10, 10), fn = hi, lower = c(-40, -40), upper = c(40, 40), control = list(temperature = 500, nb.stop.improvement = 30, maxit = 4000))[c("value", "par")])
}
mean(trace.3$fun)
table((apply(trace.3[c(2,3)], 1, paste, collapse = "/")))

## Call direct search method - Just to see that the result depends on startin values
trace.4 <- data.frame(fun = rep(NA, nloop), x1 = rep(NA, nloop), x2 = rep(NA, nloop))
for(i in c(1 : nloop)) {
  trace.4[i, ] <- unlist(optim(par = c(-10, -10), fn = hi, method = "Nelder-Mead")[c("value", "par")])
}
mean(trace.4$fun)
table((apply(trace.4[c(2,3)], 1, paste, collapse = "/")))

## -> Likelihood of covariate combination is +/- equal for optim_sa & optim, GenSA always only finds 3, 2

## Check performance ##
m1 <- microbenchmark::microbenchmark(
  optim_sa(fun = hi,
           start = (c(10, 10)),
           trace = TRUE,
           lower = c(-40, -40),
           upper=c(40, 40),
           control = list(t0 = 500,
                          nlimit = 100,
                          r = 0.8,
                          rf = 3,
                          ac_acc = 0.1,
                          dyn_rf = TRUE
           )
  )
)

m2 <- microbenchmark::microbenchmark(
  optim(par = c(10, 10), fn = hi, method = "SANN", control = list(tmax = 500, maxit = 3900, reltol = 0.1, temp = 100, trace = TRUE))
)

m3 <- microbenchmark::microbenchmark(
  GenSA::GenSA(par = c(-2.804, -3.131), fn = hi, lower = c(-40, -40), upper = c(40, 40), control = list(temperature = 500))
)

boxplot(cbind(m1$time, m2$time, m3$time))
