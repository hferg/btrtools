#' ggTrace
#'
#' Make a trace plot for an MCMC parameter using ggplot2.
#' @param pars The paramater you want to see an autocorrelation plot for.

ggTrace <- function(pars){
  z <- ggplot(data.frame(Iteration = c(1:length(pars)), p = pars), aes(x = Iteration, y = p)) +
    geom_line(color = "dodgerblue") +
    theme(plot.background = element_rect(color = "darkgray"))
  return(z)
}

