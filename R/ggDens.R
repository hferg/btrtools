#' ggDens
#'
#' Make a density plot for an MCMC parameter using ggplot2.
#' @param dat The vector of paramater you want to see an autocorrelation plot for.

ggDens <- function(output, pars, title = ""){
  dat <- output[ ,pars]
  bw <- 1.06 * min(sd(dat), IQR(dat)/1.34) * length(dat)^-0.2
  z <- ggplot(data.frame(p = dat), aes(x = p)) +
    geom_density(alpha = 0.3, fill = "dodgerblue", binwidth = bw) +
    ggtitle(paste(pars, title))
 
  return(z)
}

