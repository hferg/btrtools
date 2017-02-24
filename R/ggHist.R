#' ggHist
#' 
#' A function to plot a nice histogram - one that uses ggplot2
#' but has sensible break behaviour in R. Produces a single
#' histogram but is mostly called internally for things like
#' plotPosts.
#' @name ggHist
#' @param parameter The a vecotr of values that you want a histogram of.
#' @param fill The colour to fill the bars with
#' @param breaks The algorithgm to calculate breaks. Defaults to "scott", can also be "fandd"
#' @param title The title of the plot, defaults to off.
#' @return A histogram with nice ggplot aesthetics, and good bin widths.

ggHist <- function(output, pars, fill = "dodgerblue", breaks = "scott", title = "") {
  dat <- output[ ,pars]
  dat <- dat[!is.na(dat)]
  
  if (breaks == "scott") {
    bwidth <- 3.5 * sd(dat) * length(dat) ^ -(1/3)
  } else if (breaks == "fandd") {
    bwidth <- 2 * IQR(dat) * length(dat) ^ -(1/3)
  } else {
    bwidth = (max(dat) - min(dat))/30
  }
    
  if (fill == "count") {
    z <- ggplot(data.frame(parameter = dat), aes(x = parameter, fill = ..count..)) +
      geom_histogram(color = "darkgray", binwidth = bwidth) +
      ggtitle(paste(pars, title))
  } else {
    z <- ggplot(data.frame(parameter = dat), aes(x = parameter)) +
      geom_histogram(color = "darkgray", fill = fill, binwidth = bwidth) +
      ggtitle(paste(pars, title))
  }
  return(z)
}
