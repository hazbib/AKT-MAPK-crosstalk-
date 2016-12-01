
library(deSolve)
mmk <- function(lambda, y, parms){
  dy1 <- -337.23*y[1]*(0.000079433-y[2]) + 0.0909*y[2] + 0.9091*y[4] 
  dy2 <- 337.23*y[1]*(0.000079433-y[2]) - (0.0909+0.9091)*y[2]
  dy3 <- -1841*y[3]*(0.0003283-y[4]) + 0.0909*y[4] + 0.9091*y[2]
  dy4 <- 1841*y[3]*(0.0003283-y[4]) - (0.0909+0.9091)*y[4]
  dy5 <- lambda + (1 - 0.01*0.559)*y[3] - y[5] 
  list(c(dy1, dy2, dy3, dy4, dy5))
}

#a1 = 337.23 
#a2 = 1841
#d1 = d2 = 0.0909
#k1 = k2 = 0.9091
#phi = 1 
#eps = 0.01
#psi = 0.559 
#E1 = 0.000079433
#E2 = 0.0003283

yi <- c(y1 = 1, y2 = 0, y3 = 0, y4 = 0, y5 = 1)
lambda <- seq(0,1,0.01)

out <- ode(times = lambda, y = yi, func = mmk, parms = NULL)

head (out, n=100) 
plot(out)
plot(out[,'time'], out[,'y1'])
plot(out[,'time'], out[,'y5'])

# y1 <- [AKT]
# y2 <- [AKT:E1]
# y3 <- [pAKT]
# y4 <- [pAKT:E2]
# y5 <- [pIRS1]
# time <- lambda 
