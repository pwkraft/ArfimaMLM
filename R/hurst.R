hurst <- 
  function (x, method = "standard", freq.max = 0.25, dc = FALSE, 
          n.block = NULL, weight = function(x) rep(1, length(x)), fit = lm, 
          sdf.method = "multitaper") 
{
  # This function is a modified version of hurstSpec in the fractal-package written by
  # William Constantine and Donald Perciva. It was modified for internal use as part of
  # the arfimaMLM package
  x <- diff(x) # differencing series before estimation of Hurst exponent
  checkScalarType(method, "character")
  method <- match.arg(lowerCase(method), c("standard", "smoothed", 
                                           "robinson"))
  checkScalarType(freq.max, "numeric")
  if (freq.max <= 0 || freq.max >= 0.5) 
    stop("freq.max must be on the normalized frequency range (0,0.5)")
  if (!is.function(fit)) 
    stop("fit must be a linear regression function")
  fitstr <- deparse(substitute(fit))
  supported <- c("lm", "lmsreg", "ltsreg")
  if (!is.element(fitstr, supported)) 
    stop("Supported linear regression functions are:", supported)
  lmfit <- is.element(fitstr, "lm")
  data.name <- deparse(substitute(x))
  if (is.element(method, "smoothed")) 
    dc <- FALSE
  sdf <- sapa::SDF(x, method = sdf.method, single.sided = TRUE, 
                   npad = length(x))
  attr(sdf, "series.name") <- data.name
  df <- attr(sdf, "deltaf")
  freq.indx.max <- ceiling(freq.max/df)
  freq.indx.min <- ifelse1(dc, 1, 2)
  if (is.element(method, c("standard", "smoothed"))) {
    xvar <- log10(freq.indx.min:freq.indx.max)
    yvar <- log10(sdf[freq.indx.min:freq.indx.max])
    if (is.element(method, "smoothed")) {
      if (is.null(n.block)) 
        n.block <- as.integer(floor(logb(length(x), base = 2)))
      checkScalarType(n.block, "integer")
      lxvar <- length(xvar)
      delx <- (xvar[lxvar] - xvar[1])/n.block
      xbox <- c(xvar[1] + ((1:n.block) - 0.5) * delx)
      xbdy <- as.numeric(c(xvar[1], (xbox + 0.5 * delx)))
      ybox <- vector(mode = "double", length = n.block)
      for (k in 1:n.block) {
        ybox[k] <- mean(yvar[(xvar >= xbdy[k]) & (xvar <= 
                                                    xbdy[k + 1])])
      }
      xvar <- xbox
      yvar <- ybox
    }
    w <- NULL
    logfit <- ifelse1(lmfit, fit(y ~ 1 + x, data = data.frame(x = xvar, 
                                                              y = yvar, w = weight(yvar)), weights = w), fit(y ~ 
                                                                                                               1 + x, data = data.frame(x = xvar, y = yvar)))
    exponent <- logfit$coefficients["x"]
    H <- (1 - exponent)/2
  }
  else if (is.element(method, "robinson")) {
    n.freq <- length(sdf)
    spec.low <- sdf[1:freq.indx.max]
    csumspec <- cumsum(spec.low)
    parmq <- 0.6
    delH <- 1
    while (abs(delH) > 0.001) {
      qm <- floor(parmq * freq.indx.max)
      NewH1 <- 1 - log(csumspec[qm]/csumspec[freq.indx.max])/(2 * 
                                                                log(parmq))
      if (NewH1 >= 0.75) {
        NewH1 <- 1 - (((pi * qm)/n.freq) * mean(spec.low[(qm - 
                                                            2):qm]))/csumspec[qm]
        if (NewH1 >= 0.75) {
          AvgH <- NewH1
          parmq <- 1
        }
        else {
          AvgH <- NewH1 - 1
          parmq <- 0.9 * parmq
        }
      }
      else {
        NewH2 <- 0.75 - exp((0.01829 - parmq)/0.1292)
        AvgH <- mean(c(NewH1, NewH2))
        parmq <- 0.01829 - 0.1292 * log(0.75 - AvgH)
      }
      delH <- NewH1 - AvgH
      xvar <- yvar <- logfit <- NULL
    }
    H <- NewH1
  }
  names(H) <- "H"
  #fractalBlock(domain = "frequency", estimator = "Hurst coefficient via regression of nonparametric SDF estimate", 
  #             exponent = H, exponent.name = "H", scale = xvar, stat = yvar, 
  #             stat.name = switch(method, standard = "SDF", smoothed = "mean of SDF blocks", 
  #                                robinson = "Robinson Integration"), detrend = NULL, 
  #             overlap = NULL, data.name = data.name, sum.order = 0, 
  #             series = asVector(x), logfit = logfit, sdf = sdf)
  d <- H - 0.5 + 1 # +1 for diff!
  out <- list(d=d)
  return(out)
}