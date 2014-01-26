print.summary.arfimaMLM <-
function(x, ...){
  if(!is.null(x$ecm)){
    cat("\n#######################################\n")
    cat("Summary for Error Correction Mechanism: \n")
    print(x$ecm)
  }
  cat("\n###################################\n")
  cat("Fractional Differencing Parameters: \n\n")
  print(x$d)
  cat("\n\n#############################\n")
  cat("Summary for Multilevel Model: \n\n")
  print(x$result)
}
