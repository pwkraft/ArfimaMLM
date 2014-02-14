fd.default <-
function(x, dval="Hurst") {  
  d.value <- NA
  if(dval=="Hurst") d.value <- hurst(x, method="standard", sdf.method="multitaper")
  else if(dval=="ML") d.value <- fracdiff(x)
  else if(dval=="GPH")  d.value <- fdGPH(x)
  else if(dval=="Sperio")  d.value <- fdSperio(x)
  else stop("Fractional differencing estimator must be 'Hurst', 'ML', 'GPH', or 'Sperio'")
  x <- diffseries(x, d.value$d)
  out <- list(series=x,estimator=dval,value=d.value$d)
  out
}
