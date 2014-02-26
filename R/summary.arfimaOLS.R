summary.arfimaOLS <-
function(object, ...){
  d<- object$d
  result <- summary(object$result, ...)
  if(!is.null(object$ecm)){
    ecm <- summary(object$ecm, ...)
    out <- list(ecm=ecm,d=d,result=result)
  }
  else out <- list(d=d,result=result)
  class(out) <- "summary.arfimaOLS"
  out
}
