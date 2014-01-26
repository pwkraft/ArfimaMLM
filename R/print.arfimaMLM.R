print.arfimaMLM <-
function(x, ...){
  if(!is.null(x$ecm)){
    cat("\n######################################\n")
    cat("Result for Error Correction Mechanism: \n")
    print(x$ecm)
  }
  cat("\n###################################\n")
  cat("Fractional Differencing Parameters: \n\n")
  print(x$d)
  cat("\n\n############################\n")
  cat("Result for Multilevel Model: \n\n")
  print(x$result)
}
