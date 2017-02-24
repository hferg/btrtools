#' viewLogfile
#'
#' A simple function that shows the logfile.
#' @param logfile The name of the logfile.
#' @param n The number of rows of the logfile to view (defaults to the whole file)
#' @param thinning Thinning parameter for the posterior - defaults to 1 (all samples). 2 uses every second sample, 3 every third and so on.
#' @export

viewLogfile <- function(logfile, n = "max", thinning = 1, burnin = 0) {
  
  if (n == "max") {
    out <- btmcmc(logfile, thinning = thinning, burnin = burnin)
  } else {
    out <- head(btmcmc(logfile, thinning = thinning, burnin = burnin), n)
  }
  
  return(out)
}

