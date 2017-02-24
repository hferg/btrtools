#' ggTrace
#'
#' Make a trace plot for an MCMC parameter using ggplot2.
#' @param output BayesTraits MCMC output - output from btmcmc function.
#' @param pars The paramater you want to see an autocorrelation plot for.

ggTrace <- function(output, pars, title = ""){
  dat <- output[ ,pars]
  z <- ggplot(data.frame(Iteration = c(1:length(dat)), p = dat), aes(x = Iteration, y = p)) +
    geom_line(color = "dodgerblue") +
    ggtitle(paste(pars, title))
 
  return(z)
}

