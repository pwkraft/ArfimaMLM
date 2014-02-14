fd.numeric <-
function(x, dval) {
  if(dval!=0) x <- diffseries(x, dval)
  out <- list(series=x,estimator="external",value=dval)
  out
}
