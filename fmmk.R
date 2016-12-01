library(deSolve)

fmodel <- function(lambda, y, parms){
  dy1 <- -20*y[1]*(y[2]- y[1]*y[2]/(0.05+y[1])) + 0.0909*y[1]*y[2]/(0.05+y[1]) + 0.9091*(1-y[1])/(0.05+(1-y[1]))
  dy2 <- lambda + (1 - 0.01*0.559 -0.559*338.44*0.81552)*(1-y[1]) - 420*0.81552 - y[2]
  list(c(dy1, dy2))
}

# d1 = 0.0909
# k1 = k2 = 0.9091
# K_1 = K_2 = 0.05
# beta = 1  
# E2t = 1 
# phi = 1 
# eps = 0.01
# psi = 0.559
# kk2 = 338.44
# kk3 = 420
# sig = 1
# erk = 0.81552

# (d1+k1)/K_1 = 20 


yj <- c(y1 = 1, y2 = 1)
lambda <- seq(0,1,0.1)
time = seq(0,1,0.01)

outf <- ode(times = time, y = yj, func = fmodel, parms = NULL)

plot(outf)