#' paramDiagnostic
#'
#' Plots 4 MCMC diagnostics for a parameter
#' @param logfile The name of the logfile of the BayesTraits run
#' @param pars A vector of the names of the parameters to generate diagnostic plots for.
#' @param type The type of diagnostic plot, if plotting across many parameters. Either "autocor", "dens", "trace" or "runmean"
#' @param cols The number of columns for multiple plotting.
#' @param thinning Thinning parameter for the posterior - defaults to 1 (all samples). 2 uses every second sample, 3 every third and so on.
#' @param burnin The number of generations to remove from the start of the chain as burnin.
#' @keywords mcmc diagnostic autocorrelation trace running mean density
#' @export
#' @examples
#' paramDiagnostic("cool-data.log", "Lh")
#' params <- getParams("cool-data.log")
#' paramDiagnostic("cool-data.log", params, type = "trace", cols = 3)

paramDiagnostic <- function(logfile, pars, type = NULL, cols = 2, thinning = 1, burnin = 0,
  title = NULL) {
  
  if (is.data.frame(logfile)) {
    output <- logfile
  } else { 
    output <- btmcmc(logfile, thinning = thinning, burnin = burnin)
  }

  if (length(pars) == 1) {
    ac <- ggAutoCor(output, pars, conf = 0.95, min.lag = 1, title = title)
    hs <- ggHist(output, pars, title = title)
    tc <- ggTrace(output, pars, title = title)
    rm <- ggRunmean(output, pars, title = title, window.size = 10)
    plots <- list(ac, hs, tc, rm)
  } else {
    
    if (is.null(type)) {
      return(print("Must specify plot type"))
    }
  
    plots <- list()

    for (i in 1:length(pars)) {
      if (type == "autocor") {
        plots[[i]] <- ggAutoCor(output, pars[i], conf = 0.95, min.lag = 1)
      } else if (type == "dens") {
        plots[[i]] <- ggDens(output, pars[i], title = title)
      } else if (type == "trace") {
        plots[[i]] <- ggTrace(output, pars[i], title = title)
      } else if (type == "runmean") {
        plots[[i]] <- ggRunmean(output, pars[i], title = title, window.size = 10)
      } else if (type == "hist") {
        plots[[i]] <- ggHist(output, pars[[i]], title = title)
      }
      
    }
  }
  
  return(suppressWarnings(multiplot(plotlist = plots, cols = cols)))
}

