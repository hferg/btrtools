#' ggRunmean
#'
#' Make a running mean plot for an MCMC parameter using ggplot2.
#' @param dat The vector of the paramater you want to see an autocorrelation plot for.

ggRunmean <- function(output, pars){
  runmeans <- cumsum(output[ ,pars])/seq(along = output[, pars])
  z <- ggplot(data.frame(Iteration = c(1:length(runmeans)), runningmean = runmeans), aes(x = Iteration, y = runningmean)) +
    geom_line(color = "dodgerblue") +
    ggtitle(paste(pars))
  return(z)
}

