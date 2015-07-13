#' plotPosts
#'
#' A function to plot one or more posterior distributions from the logfile of a BayesTraits
#' analysis.
#'
#' @param logfile The name of the trait data file on which BayesTraits was run.
#' @param params A vector containing the name of the parameter(s) that are to be plotted. Can be
#' subsetted from the output of getParams().
#' @param fill The colour for the bars of the histogram. If "count" histogram will be shaded according to count.
#' @param cols The number of columns to plot multiple plots into.
#' @param thinning Thinning parameter for the posterior - defaults to 1 (all samples). 2 uses every second sample, 3 every third and so on.
#' @param burnin The number of generations to remove from the start of the chain as burnin.
#' @keywords plot posterior histogram distribution
#' @export
#' @examples
#' plotPosterior(cool-data.txt, c("Lh", "Alpha 1"))
#' plotPosterior(cool-data.txt, params[c(1:2)])

plotPosts <- function(logfile, pars, fill = "dodgerblue", cols = 2, thinning = 1, burnin = 0) {
  output <- btmcmc(logfile, thinning = thinning, burnin = burnin)
  
  if (length(pars) == 1) {
    bwidth <- 3.5 * sd(output[ ,pars]) * length(output[ ,pars]) ^ -(1/3)
 
    if (fill == "count") {
      ret <- ggplot(data.frame(p = output[ ,pars]), aes(x = p, fill = ..count..)) +
        geom_histogram(color = "darkgray", binwidth = bwidth) +
        scale_x_continuous(paste(pars))
    } else {
      ret <- ggplot(data.frame(p = output[ ,pars]), aes(x = p)) +
        geom_histogram(color = "darkgray", binwidth = bwidth, fill = fill) +
        scale_x_continuous(paste(pars))
    }
    
  return(suppressWarnings(ret))
  } else {
    plots <- list()
    
    for (i in 1:length(pars)) {
      bwidth <- 3.5 * sd(output[ ,pars[i]]) * length(output[ ,pars[i]]) ^ -(1/3)
      
      if (bwidth == 0) {
        bwidth <- 1
        }
      
      plots[[i]] <- ggplot(data.frame(p = output[ ,pars[i]]), aes(x = p)) +
        geom_histogram(color = "darkgray", binwidth = bwidth, fill = fill) +
        scale_x_continuous(paste(pars[i]))
    }
    
    return(suppressWarnings(multiplot(plotlist = plots, cols = cols)))
  }
}

