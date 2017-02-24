#' paramLh
#'
#' Plots a parameter against the likelihood for each iteration.
#' @param logfile The name of the logfile of the BayesTraits run
#' @param pars A vector of the names of the parameters to be plotted against likelihood.
#' @param cols The number of columns of the resultant plot (if more than one plot)
#' @param thinning Thinning parameter for the posterior - defaults to 1 (all samples). 2 uses every second sample, 3 every third and so on.
#' @param burnin The number of generations to remove from the start of the chain as burnin.
#' @export
#' @keywords parameters log-likelihood
#' @examples
#' paramsLh("cool-data.log", "Lambda")
#' paramsLh("cool-data.log", getParams("cool-data.log"))
#' params <- getParams("cool-data.log")
#' paramLh("cool-data.log", params[c(2:4)])

paramLh <- function(logfile, pars, cols = 2, thinning = 1, burnin = 0) {
  output <- btmcmc(logfile, thinning = thinning, burnin = burnin)
  
  if (length(pars) == 1) {
    p <- data.frame(Lh = output[ ,"Lh"], output[ ,pars])
    colnames(p) <- c("Lh", pars)
    ret <- ggplot(p, aes_string(x = pars, y = "Lh")) +
      geom_point(colour = "dodgerblue")
    return(ret)
  } else {
    p <- data.frame(Lh = output[ ,"Lh"], output[ ,pars])
    plots <- list()
    
    for (i in 1:length(pars)) {
      plots[[i]] <- ggplot(p, aes_string(x = pars[i], y = "Lh")) +
        geom_point(colour = "dodgerblue")
    }
    
  return(multiplot(plotlist = plots, cols = cols))
  }
}

