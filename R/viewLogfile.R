#' viewLogfile
#'
#' A simple function that shows the logfile.
#' @param logfile The name of the logfile.
#' @param n 
#' @export

viewLogfile <- function(logfile, n = "max") {
  
  if (n == "max") {
    out <- btmcmc(logfile)
  } else {
    out <- head(btmcmc(logfile), n)
  }
  
  return(out)
}

