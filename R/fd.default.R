fd.default <-
function(x, dval="GPH", ...) {  
  d.value <- NA
  if(dval=="ML") d.value <- fracdiff(x, ...)
  else if(dval=="GPH")  d.value <- fdGPH(x, ...)
  else if(dval=="Sperio")  d.value <- fdSperio(x, ...)
  else stop("Fractional differencing estimator must be 'ML', 'GPH', or 'Sperio'")
  x <- diffseries(x, d.value$d)
  out <- list(series=x,estimator=dval,value=d.value$d)
  out
}
