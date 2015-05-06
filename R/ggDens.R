#' ggDens
#'
#' Make a density plot for an MCMC parameter using ggplot2.
#' @param pars The paramater you want to see an autocorrelation plot for.

ggDens <- function(pars){
  bw <- 1.06 * min(sd(pars), IQR(pars)/1.34) * length(pars)^-0.2
  z <- ggplot(data.frame(p = pars), aes(x = p)) +
    geom_density(alpha = 0.3, fill = "dodgerblue", binwidth = bw) +
    theme(plot.background = element_rect(color = "darkgray"))
  return(z)
}

