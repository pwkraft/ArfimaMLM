# example arfimaMLM
rm(list=ls())
detach("package:ArfimaMLM", unload=TRUE)
require(fracdiff)
require(lme4)
require(fractal)
setwd("/data/Copy/packages/ArfimaMLM/ArfimaMLM/R")
for(func in dir()){
  source(func)
}
rm(func)

# set basic parameters for simulation
t = 100 # number of time points
n = 500 # number of observations within time point
N = t*n # total number of observations

# generate fractional ARIMA Time Series for y_t, x1_t, z1_t, z2_t
set.seed(123)
y_t <- fracdiff.sim(t, d=0.4, mu=10)$series
x1_t <- fracdiff.sim(t, d=0.3, mu=5)$series
z1_t <- fracdiff.sim(t, d=0.1, mu=2)$series
z2_t <- fracdiff.sim(t, d=0.25, mu=3)$series

# simulate data
data <- NULL; data$time <- rep(seq(1:t),each=n)
data <- data.frame(data)
data$x1 <- rnorm(N,rep(x1_t,each=n),2)
data$x2 <- rnorm(N,0,40)
data$z1 <- rnorm(N,rep(z1_t,each=n),3)
data$z2 <- rep(z2_t,each=n)
b1 <- 0.2+rep(rnorm(t,0,0.1),each=n)
data$y <- (b1*data$x1-0.05*data$x2+0.3*rep(z1_t,each=n)
           +0*data$z2+rnorm(N,rep(y_t,each=n),1))

# estimate models
m1 <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd +(1|time), timevar="time"
                ,data=data ,d="ML")
m1b <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd +(1|time), timevar="time"
                ,data=data ,d=0)
m2 <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd + ecm + (1|time), timevar="time"
                , ecmformula = y.mean ~ x1.mean
                , d=list(y="ML", z1="Sperio", z2=0.25)
                , decm="ML", data=data)
m3 <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd + (1+x1.xdif|time), timevar="time"
                , arma=list(y=c(2,0),z2=c(0,1)), data=data, REML=T)
m4a <- arfimaMLM(y ~ x1 + x2 + z1 + z2 + (1|time), timevar="time"
                , data=data, REML=T)
m4b <- arfimaMLM(y.ydif ~ x1 + x2 + z1 + z2 + (1|time), timevar="time"
                , data=data, REML=T)
m4c <- arfimaMLM(y ~ x1.xdif + x2 + z1 + z2 + (1|time), timevar="time"
                , data=data, REML=T)
m4d <- arfimaMLM(y ~ x1.fd + x2 + z1 + z2 + (1|time), timevar="time"
                 , data=data, REML=T)
m5 <- arfimaOLS(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd, timevar="time"
                ,data=data ,d="ML")
m6 <- arfimaOLS(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd + ecm, timevar="time"
                , ecmformula = y.mean ~ x1.mean
                , d=list(y="ML", z1="Sperio", z2=0.25)
                , decm="ML", data=data)
m7 <- arfimaOLS(y ~ x1 + x2 + z1.fd + z2 + ecm, timevar="time"
                , ecmformula = y.mean ~ x1.mean
                , decm="ML", data=data)
data <- data[data$time!=58,]
m8 <- arfimaOLS(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd, timevar="time"
                , ecmformula = y.mean ~ x1.mean
                , arma=list(y=list(1,c(1,3)),z2=list(0,c(1,2))), data=data)
summary(m8)
summary(m3)
summary(m3$result)

# example for fd function
set.seed(123)
series <- fracdiff.sim(100, d=0.4, mu=10)$series
series.fd <- fd(series, dval="ML")
series.fd <- fd(series, dval="GPH")
series.fd <- fd(series, dval=0.4)

# compare with Arfima 1.1
m9 <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd + ecm + (1|time), timevar="time"
                , ecmformula = y.mean ~ x1.mean
                , d=list(y="ML", z1="Sperio", z2=0.25)
                , decm="ML", data=data)
rm(arfimaMLM)
library(ArfimaMLM)
m10 <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd + ecm + (1|time), timevar="time"
                , ecmformula = y.mean ~ x1.mean
                , d=list(y="ML", z1="Sperio", z2=0.25)
                , decm="ML", data=data, drop=5)

summary(m9)
summary(m10)

### test github version

rm(list=ls())
library(devtools)
install_github("pwkraft/ArfimaMLM")
library(ArfimaMLM)

# set basic parameters for simulation
t = 100 # number of time points
n = 500 # number of observations within time point
N = t*n # total number of observations

# generate fractional ARIMA Time Series for y_t, x1_t, z1_t, z2_t
set.seed(123)
y_t <- fracdiff.sim(t, d=0.4, mu=10)$series
x1_t <- fracdiff.sim(t, d=0.3, mu=5)$series
z1_t <- fracdiff.sim(t, d=0.1, mu=2)$series
z2_t <- fracdiff.sim(t, d=0.25, mu=3)$series

# simulate data
data <- NULL; data$time <- rep(seq(1:t),each=n)
data <- data.frame(data)
data$x1 <- rnorm(N,rep(x1_t,each=n),2)
data$x2 <- rnorm(N,0,40)
data$z1 <- rnorm(N,rep(z1_t,each=n),3)
data$z2 <- rep(z2_t,each=n)
b1 <- 0.2+rep(rnorm(t,0,0.1),each=n)
data$y <- (b1*data$x1-0.05*data$x2+0.3*rep(z1_t,each=n)
           +0*data$z2+rnorm(N,rep(y_t,each=n),1))

# estimate models
m1 <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd +(1|time), timevar="time"
                ,data=data ,d="ML")
m1b <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd +(1|time), timevar="time"
                 ,data=data ,d=0)
