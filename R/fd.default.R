#' @export
#' @importFrom fracdiff fracdiff
#' @importFrom fracdiff fdGPH
#' @importFrom fracdiff fdSperio
#' @importFrom fracdiff diffseries
#' @importFrom stats aggregate arima coef lm na.omit residuals
#' @import ggplot2
fd.default <-
  function(x, dval="ML", ...) {  
  d.value <- NA
  if(dval=="Hurst"){
    requireNamespace("fractal", quietly=TRUE)
    d.value <- fractal::hurstSpec(diff(x), method="standard", sdf.method="multitaper", ...)[1]+.5
  }
  else if(dval=="ML") d.value <- fracdiff(x, ...)$d
  else if(dval=="GPH")  d.value <- fdGPH(x, ...)$d
  else if(dval=="Sperio")  d.value <- fdSperio(x, ...)$d
  else stop("Fractional differencing estimator must be 'Hurst', 'ML', 'GPH', or 'Sperio'")
  if (d.value > 1) d.value <- 1
  x <- diffseries(x, d.value)
  out <- list(series=x,estimator=dval,value=d.value)
  out
}
