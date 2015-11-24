#' @export whittleFML
#' @importFrom arfima tacvfARFIMA

# ------------------------------------------ #
# Autocovariances: calculate theoretical auto-
# covariance functions of FI and ARMA series 
# and mix. 
# -------------------------------------------#
DLResiduals <- function(r,z) {
    n <- length(z)
    error <- numeric(n)
    sigmasq <- numeric(n)
    error[1] <- z[1]
    sigmasq[1] <- r[1]
    phi <- r[2]/r[1]
    error[2] <- z[2] - phi * z[1]
    sigmasqkm1 <- r[1] * (1 - phi^2)
    sigmasq[2] <- sigmasqkm1
    for(k in 2:(n - 1)) {
      phikk <- (r[k + 1] - phi %*% rev(r[2:k]))/sigmasqkm1
      sigmasqk <- sigmasqkm1 * (1 - phikk^2)
      phinew <- phi - phikk * rev(phi)
      phi <- c(phinew, phikk)
      sigmasqkm1 <- sigmasqk
      error[k + 1] <- z[k + 1] - crossprod(phi, rev(z[1:k]))
      sigmasq[k + 1] <- sigmasqk
    }
    res<-error
    res 
}

# ------------------------------------------ #
# Estimating the spectral density 
# ------------------------------------------ #
specDens <- function(params, p,q, n) {
  # At Fourier frequencies 2*pi*j/m (j = floor(m/2))
  # n = length of time series
  
  j <- (n-1) %/% 2
  d <- params[1]
  
  ## Estimate the Fourier frequencies
  fs <- 2*pi*(1:j)/n
  
  ## Calculate the spectral density function
  if(p > 0) {
    phi <- params[2:(p+1)]
    px <- outer(fs, 1:p)
    Rar <- cos(px) %*% phi # matrix product
    Iar <- sin(px) %*% phi
    
    far <- (1-Rar)^2 + Iar^2
  } else {
    phi <- numeric()
    far <- 1
  }
  
  if(q > 0) {
    theta <- params[(p+2):(p+q+1)]
    px <- outer(fs, 1:q)
    Rma <- cos(px) %*% theta
    Ima <- sin(px) %*% theta
    
    fma <- (1+Rma)^2 + Ima^2
  } else {
    theta <- numeric()
    fma <- 1
  }
  
  ## Spectral Density: 
  sif <- (2 - 2*cos(fs))^(-d)*fma/far  
  
  r <- list(fs = fs, sif = sif,
            pq = c(p,q), params = c(d = d, phi = phi, theta = theta))
}

# ------------------------------------------ #
# whitFunc - function to be minimized by MLE
# ------------------------------------------ #
whitFunc <- function(params, n, In, pq=pq, give.w.only = FALSE) {
  
  p <- pq[1]
  q <- pq[2]
  
  spD <- specDens(params,p,q,n)
  sif <- spD$sif
  fy <- In/sif
  
  ## w: criterion function from Hauser (1999)
  K <- length(sif)
  w <- K*(log(2*pi) -1 + log(sum(fy)/K)) + sum(log(sif)) 
  
  if(give.w.only)
    return(w)
  
  list(n = n, d = params[1], params = params, w = w, sif = sif)
}

# ------------------------------------------ #
# Frequency Domain MLE - Whittle Estimator
# ------------------------------------------ #
whittleFML <- function(x, p, q, n=length(x), 
                       inits = list(d=0, AR = numeric(0), MA = numeric(0))) {
  
  # check initial call
  if(missing(p))  p <- length(inits$AR)
  else {
    stopifnot(length(inits$AR) == p)
    if(0 > (p <- as.integer(p))) stop("must have integer p >= 0")
  }
  if(missing(q))  q <- length(inits$MA)
  else {
    stopifnot(length(inits$MA) == q)
    if(0 > (q <- as.integer(q))) stop("must have integer q >= 0")
  }
  
  ## lining up parameters 
  pq <- c(p,q)
  params <- unlist(inits) # c(d, ar[1:p], ma[1:q]) # where p=0, q=0 is possible
  d <- params[1]
  n.params <- length(params)
  
  ## generate periodogram using fast fourier transform, drop 1st
  In <- (Mod(fft(x))^2/(2*pi*n)) [2:((n+1) %/% 2)]
  
  ## define function to be minimized by ML estimator 
  FMLwrap <- function(params) {
    r <- whitFunc(params, n = n, In = In, pq = pq, give.w.only = TRUE)
  }
  
  ## MLE method depends on number of parameters to estimate
  if(n.params == 1) { # one dimensional - ARFIMA (0,d,0) 
    result <- optim(par = params, fn = FMLwrap, lower = -.99, upper = .99,
                    method = "Brent", hessian = TRUE)
    params <- c(d = result$par)
  } 
  else { # ARFIMA (p,d,q)
    result <- optim(par = params, fn = FMLwrap, lower = -.99, upper = .99, 
                    method = "L-BFGS-B", hessian = TRUE)
    params <- result$par
  }
  
  ## Estimated Results
  whittle <- whitFunc(params, n, In, pq)
  
  if  (pq[1] == 0 && pq[2] == 0) {
        phi <- 0     
        theta <- 0
      }
      else if (pq[1] == 0 && pq[2] != 0) { 
        theta = params[2:(sum(pq)+1)]
        phi = 0
      }
      else if (pq[1] != 0 && pq[2] != 0) {
        phi = params[2:(2+pq[1]-1)]
        theta = params[(2+pq[1]):(sum(pq)+1)]
      }
      else if (pq[1] != 0 && pq[2] == 0) {
        phi = params[2:2+pq[1]-1]
        theta = 0;       
      }
  d <- params[1]
  
  Vcov <- solve(result$hessian)
  ssd <- sqrt(diag(Vcov))
  coef <- cbind(Estimate = round(params,4), "Std.Error" = round(ssd,4),
                "z value" = round(params/ssd,4),
                "Pr(>|z|)" = round(2 * pnorm(-abs(params/ssd)),4))
  dimnames(Vcov) <- list(names(params), names(params))
  
  ## Information Criteria (AIC, BIC values from Davidson (2015))
  LL <- whittle$w
  AIC <- -LL - n.params
  BIC <- -LL - (n.params* log(n))/2
  
  IC <- data.frame(rbind("Log-Likelihood" = -LL, 
                         "AIC" = AIC,
                         "BIC" = BIC))
  colnames(IC)[1] <- "IC"
  
  ## Theoretical autocovariances and residuals
  rr <- arfima::tacvfARFIMA(phi = phi, theta = theta, dfrac = d, maxlag = n-1)
  res <- DLResiduals(rr, x)
  
  ## output results
  ## print(list(coef, IC))
  
  ## save results as list 
  whit.result <- list(n = n, p=p, q=q,
                      coefficients = coef, vcov = Vcov,
                      IC = IC, In = In, sif = whittle$sif,
                      rr = rr, res = res) 
}
