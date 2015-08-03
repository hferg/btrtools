#' ggRunmean
#'
#' Make a running mean plot for an MCMC parameter using ggplot2.
#' @param dat The vector of the paramater you want to see an autocorrelation plot for.

ggRunmean <- function(output, pars, window.size = 10, title = "") {
  dat <- output[ , pars]
  windows <- seq.int(window.size, length(dat), window.size)
  
  if (windows[length(windows)] != length(dat)) {
    windows <- c(windows, length(dat))
  }

  means <- vector(mode = "numeric", length = length(windows))

  for (i in 1:length(windows)) {
    means[i] <- mean(dat[c((windows[i] - (window.size -1)):windows[i])])
  }

  z <- ggplot(data.frame(Window = c(1:length(windows)), mean = means), aes(x = Window, y = mean)) +
    geom_point(color = "dodgerblue") +
    ggtitle(paste(pars, title)) +
    geom_smooth(method = "lm", formula = y ~ x)
  return(z)
}

