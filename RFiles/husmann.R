

#----------------------------------------------------#
#### Example 1: Himmelblau, cont. parameter space ####
#----------------------------------------------------#
## Clear workspace ##
rm(list = ls())

## Load packages ##
library(GenSA)
library(optimization)
library(xtable)
library(microbenchmark)

## setwd ##
#setwd('/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/')

## Himmelblau's function ##
# 4 minima at
# f(3, 2) = 0
# f(-2.804, -3.131) = 0
# f(-3.779, -3.283) = 0
# f( 3.584, -1.848) = 0

hi <- function(x){
  (x[1]**2 + x[2] - 11)**2 + (x[1] + x[2]**2 -7)**2
}

nloop <- 10000 # Caution: Time intensive


#---------------------------#
## Frequency of covariates ##
#---------------------------#

## optim_sa ##

trace.1 <- data.frame(fun = rep(NA, nloop), x1 = rep(NA, nloop), x2 = rep(NA, nloop), meth = "optim_sa")
for(i in c(1 : nloop)) {
  trace.1[i, c(1 : 3)] <- unlist(optim_sa(fun = hi,
                                          start = (c(10, 10)),
                                          trace = TRUE,
                                          lower = c(-40, -40),
                                          upper=c(40, 40),
                                          control = list(t0 = 500,
                                                         nlimit = 50,
                                                         r = 0.85,
                                                         rf = 3,
                                                         ac_acc = 0.1,
                                                         dyn_rf = TRUE
                                          )
  )[c("function_value", "par")])
}

round(mean(trace.1$fun, na.rm = TRUE), digits = 3) <= 0.001
trace.1.rnd <- cbind(round(trace.1[, c(1 : 3)], digits = 1), trace.1[, 4])
table((apply(trace.1.rnd[c(2, 3)], 1, paste, collapse = "/")))

## Call optim()  ##

trace.2 <- data.frame(fun = rep(NA, nloop), x1 = rep(NA, nloop), x2 = rep(NA, nloop),  meth = "optim_sann")
for(i in c(1 : nloop)) {
  trace.2[i, c(1 : 3)] <- unlist(optim(fn = hi, par = c(10, 10), method = "SANN", control = list(tmax = 500, reltol = 0.1, temp = 50, trace = TRUE, maxit = 7000))[c("value", "par")])
}
round(mean(trace.2$fun, na.rm = TRUE), digits = 3) <= 0.001
trace.2.rnd <- cbind(round(trace.2[, c(1 : 3)], digits = 1), trace.2[, 4])
table((apply(trace.2.rnd[c(2, 3)], 1, paste, collapse = "/")))

## Call GenSA  ##

# The example GenSA is canceled

trace.3 <- data.frame(fun = rep(NA, nloop), x1 = rep(NA, nloop), x2 = rep(NA, nloop),  meth = "GenSA")
for(i in c(1 : nloop)) {
  trace.3[i, c(1 : 3)]  <- unlist(GenSA(fn = hi, par = c(10, 10), lower = c(-40, -40), upper = c(40, 40), control = list(temperature = 50, nb.stop.improvement = 30, maxit = 500))[c("value", "par")])
}
round(mean(trace.3$fun, na.rm = TRUE), digits = 3) <= 0.001
trace.3.rnd <- cbind(round(trace.3[, c(1 : 3)], digits = 1), trace.3[, 4])
table((apply(trace.3.rnd[c(2, 3)], 1, paste, collapse = "/")))

## Call NM direct search method ##

trace.4 <- data.frame(fun = rep(NA, nloop), x1 = rep(NA, nloop), x2 = rep(NA, nloop),  meth = "optim_nm")
for(i in c(1 : nloop)) {
  trace.4[i, c(1 : 3)] <- unlist(optim(fn = hi, par = c(-10, -10),  method = "Nelder-Mead")[c("value", "par")])
}
round(mean(trace.4$fun, na.rm = TRUE), digits = 3) <= 0.001
trace.4.rnd <- cbind(round(trace.4[, c(1 : 3)], digits = 1), trace.4[, 4])
table((apply(trace.4.rnd[c(2, 3)], 1, paste, collapse = "/")))

# -> Frequency of covariate combination is +/- equal for optim_sa & optim(SANN)
# GenSA & optim(Nelder-Mead) always only finds -3.8, -3.3

## Create df with results
# Bind dfs
trace <- rbind(trace.1, trace.2, trace.4)
table(trace$meth)

# Make groups
trace$x.factor <- apply(round(trace[c(2, 3)], digits = 1), 1, paste, collapse = "/")
trace$x.factor[trace$x.factor %in% "3.6/-1.9"] <- "3.6/-1.8" # Combine -1.9 and -1.8 to one factor as the real solution is in between(approx. -1.85)
trace$x.factor <- factor(trace$x.factor)
table(trace$x.factor)

cross.table.x <- xtabs(~meth + x.factor, data = trace)
xtable(cross.table.x / 100) # LaTex Table.

# To reproduce the exact results, the workspace is stored in the following file:
# save.image(file = '/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/RFiles/Ex1_frequency.RData')
# load(file = '/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/RFiles/Ex1_frequency.RData')

#---------------#
## Performance ##
#---------------#

## Calculation ##
mb.1 <- microbenchmark(
  optim_sa(fun = hi,
           start = (c(10, 10)),
           trace = FALSE,
           lower = c(-40, -40),
           upper=c(40, 40),
           control = list(t0 = 500,
                          nlimit = 50,
                          r = 0.85,
                          rf = 3,
                          ac_acc = 0.1,
                          dyn_rf = TRUE
           )
  ), times = nloop
)


mb.2 <- microbenchmark(
  optim(par = c(10, 10), fn = hi, method = "SANN", control = list(tmax = 500, reltol = 0.1, temp = 50, trace = FALSE)), times = nloop
)

mb.3 <- microbenchmark(
  GenSA(par = c(10, 10), fn = hi, lower = c(-40, -40), upper = c(40, 40), control = list(temperature = 50, nb.stop.improvement = 30, maxit = 500)), times = nloop
)

mb.4 <- microbenchmark(
  optim(par = c(-10, -10), fn = hi, method = "Nelder-Mead"), times = nloop
)

## Visualization & saving ##

# To reproduce the exact results, the workspace is stored in the following file:
# save.image(file = '/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/RFiles/Ex1_speed.RData')
# load(file = '/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/RFiles/Ex1_speed.RData')
boxplot(cbind(mb.4$time, mb.1$time, mb.2$time))

# Counting outliers
length(mb.1$time[mb.1$time > 4e7])
length(mb.2$time[mb.2$time > 4e7])
# length(mb.3$time[mb.3$time > 4e7])
length(mb.4$time[mb.4$time > 4e7])

cex.plot.tex <- 1.6
tikzDevice::tikz('Fig/fig1_ex1-time.tex', h = 6, w = 6)
par(mar = c(6, 6, 2, 2) + 0.1)
boxplot(cbind(mb.4$time, mb.1$time, mb.2$time), ylim = c(0, 4e7), axes = FALSE)
axis(1, labels = FALSE, lwd = 0, lwd.ticks = 1)
mtext(side = 2, line = 4, "Calculation time [millisecond]", cex = cex.plot.tex)
mtext(side = 1, line = c(1.5, 3, 1.5, 3), at = c(1 : 4), c("optim (NM)", "optim\\_sa", "optim (SA)", "GenSA"), cex = cex.plot.tex)
axis(2, las = 2, labels = FALSE, lwd = 0, lwd.ticks = 1)
mtext(side = 2, line = 1.5, c(0 : 4), cex = cex.plot.tex, at = seq(0, 4e7, 1e7), las = 2)
box()
dev.off()

#-------------------------------#
#### Frequency of iterations ####
#-------------------------------#

## optim_sa ##
freq.1 <- data.frame(n_iter = rep(NA, nloop), meth = "optim_sa")
for(i in c(1 : nloop)) {
  freq.1[i, c(1)] <- sum(as.data.frame(
    optim_sa(fun = hi,
             start = (c(10, 10)),
             trace = TRUE,
             lower = c(-40, -40),
             upper=c(40, 40),
             control = list(t0 = 500,
                            nlimit = 50,
                            r = 0.85,
                            rf = 3,
                            ac_acc = 0.1,
                            dyn_rf = TRUE
             )
    )[c("trace")])$trace.n_inner)
}

## optim()  ##
freq.2 <- data.frame(n_iter = rep(NA, nloop), meth = "optim_sann")
for(i in c(1 : nloop)) {
  freq.2$n_iter[i] <- optim(fn = hi, par = c(10, 10), method = "SANN", control = list(tmax = 500, reltol = 0.1, temp = 50, trace = TRUE))$counts[1]
  # Always 9999
}


## Call GenSA  ##
freq.3 <- data.frame(n_iter = rep(9999, nloop), meth = "GenSA")
for(i in c(1 : nloop)) {
  freq.3$n_iter[i] <- GenSA(fn = hi, par = c(10, 10), lower = c(-40, -40), upper = c(40, 40), control = list(temperature = 50, nb.stop.improvement = 30, maxit = 500))$counts
}

freq.4 <- data.frame(n_iter = rep(9999, nloop), meth = "optim_nm")
for(i in c(1 : nloop)) {
  freq.4$n_iter[i] <- optim(fn = hi, par = c(-10, -10),  method = "Nelder-Mead")$counts[1]
}
freq <- rbind(freq.4, freq.1, freq.2)


## Visualization & saving ##

# To reproduce the exact results, the workspace is stored in the following file:
# save.image(file = '/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/RFiles/Ex1_count.RData')
# load(file = '/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/RFiles/Ex1_count.RData')


cex.plot.tex <- 1.6
tikzDevice::tikz('Fig/fig1_ex1-counts.tex', h = 6, w = 6)
par(mar = c(6, 6, 2, 2) + 0.1)
boxplot(freq$n_iter ~ freq$meth, ylim = c(0, 1e4), axes = FALSE)
axis(1, labels = FALSE, lwd = 0, lwd.ticks = 1)
mtext(side = 2, line = 5, "Frequency of iterations", cex = cex.plot.tex)
mtext(side = 1, line = c(1.5, 3, 1.5, 3), at = c(1 : 4), c("optim (NM)", "optim\\_sa", "optim (SA)", "GenSA"), cex = cex.plot.tex)
axis(2, las = 2, labels = FALSE, lwd = 0, lwd.ticks = 1)
mtext(side = 2, line = 1.5, seq(0, 10000, 2000), cex = cex.plot.tex, at = seq(0, 10000, 2000), las = 2)
box()
dev.off()


#### Plot 2-way graphic ####
own.cex <- 1
cex.plot.tex <- 2
tikzDevice::tikz('Fig/fig1_ex1.tex', w = 14 * own.cex, h = 7 * own.cex)
par(mfcol = c(1,2))
par(mar = c(6, 5, 2, 4) + 0.1)
boxplot(cbind(mb.4$time, mb.1$time, mb.2$time), ylim = c(0, 4e7), axes = FALSE)
axis(1, labels = FALSE, lwd = 0, lwd.ticks = 1, at = c(1 : 3))
mtext(side = 2, line = 3.5, "Calculation time [millisecond]", cex = cex.plot.tex)
mtext(side = 1, line = 3, at = c(1 : 3), c("optim (NM)", "optim\\_sa", "optim (SA)"), cex = cex.plot.tex)
axis(2, las = 2, labels = FALSE, lwd = 0, lwd.ticks = 1)
mtext(side = 2, line = 1.5, c(0 : 4), cex = cex.plot.tex, at = seq(0, 4e7, 1e7), las = 2)
box()

par(mar = c(6, 5, 2, 3) + 0.1)
boxplot(freq$n_iter ~ freq$meth, ylim = c(0, 1e4), axes = FALSE)
axis(1, labels = FALSE, lwd = 0, lwd.ticks = 1, at = c(1 : 3))
mtext(side = 2, line = 5.5, "Frequency of iterations", cex = cex.plot.tex)
mtext(side = 1, line = 3, at = c(1 : 3), c("optim (NM)", "optim\\_sa", "optim (SA)"), cex = cex.plot.tex)
axis(2, las = 2, labels = FALSE, lwd = 0, lwd.ticks = 1)
mtext(side = 2, line = 1.5, seq(0, 10000, 2000), cex = cex.plot.tex, at = seq(0, 10000, 2000), las = 2)
box()
dev.off()


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
#setwd('/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/')

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

arrows(x0 = 1.5, y0 = -0.5, x1 = 3, y1  = 0, col = 'white', lwd = 4)
arrows(x0 = 1.5, y0 = -0.5, x1 = 3, y1  = 0, col = 'black', lwd = 2)
# text(x = 1.1, y = -0.5, "10", cex = 1.2, col = 'white')
text(x = 1.1, y = -0.5, "10", cex = 1, col = 'black')

arrows(x0 = 0.5, y0 = -2.5, x1 = 2, y1  = -3, col = 'white', lwd = 4)
arrows(x0 = 0.5, y0 = -2.5, x1 = 2, y1  = -3, col = 'black', lwd = 2)
# text(x = 0.1, y = -2.5, "19", cex = 1.2, col = 'white', adj = 1)
text(x = 0.1, y = -2.5, "19", cex = 1, col = 'black')

dev.off()

# To reproduce the exact results, the workspace is stored in the following file:
# save.image(file = '/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/RFiles/Ex2_Integer.RData')
# load(file = '/home/khusman1/Documents/Veroeffentlichungen/optimization_essay/RFiles/Ex2_Integer.RData')

N <- 10000
boot_int_programming <- rep(NA, N)
for (i in c(1 : N)) {
  try(boot_int_programming[i] <- optim_sa(fun = hi,
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
  )$function_value)
}
hist(boot_int_programming)
summary(factor(boot_int_programming))
# 67.28 %

## optim (SA) ##
optim(par = c(10, 10), fn = hi, gr = var_func_int_gr, method = "SANN", control = list(trace = TRUE))

