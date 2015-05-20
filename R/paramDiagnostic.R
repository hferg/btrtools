#' paramDiagnostic
#'
#' Plots 4 MCMC diagnostics for a parameter
#' @param logfile The name of the logfile of the BayesTraits run
#' @param pars A vector of the names of the parameters to generate diagnostic plots for.
#' @param type The type of diagnostic plot, if plotting across many parameters. Either "autocor", "dens", "trace" or "runmean"
#' @param cols The number of columns for multiple plotting.
#' @keywords mcmc diagnostic autocorrelation trace running mean density
#' @export
#' @examples
#' paramDiagnostic("cool-data.log", "Lh")
#' params <- getParams("cool-data.log")
#' paramDiagnostic("cool-data.log", params, type = "trace", cols = 3)

paramDiagnostic <- function(logfile, pars, type = NULL, cols = 2) {
  output <- btmcmc(logfile)
  
  if (length(pars) == 1) {
    ac <- ggAutoCor(output, pars, conf = 0.95, min.lag = 1)
    dn <- ggDens(output, pars)
    tc <- ggTrace(output, pars)
    rm <- ggRunmean(output, pars)
    plots <- list(ac, dn, tc, rm)
  } else {
    
    if (is.null(type)) {
      return(print("Must specify plot type"))
    }
  
    plots <- list()

    for (i in 1:length(pars)) {
      if (type == "autocor") {
        plots[[i]] <- ggAutoCor(output, pars[i], conf = 0.95, min.lag = 1)
      } else if (type == "dens") {
        plots[[i]] <- ggDens(output, pars[i])
      } else if (type == "trace") {
        plots[[i]] <- ggTrace(output, pars[i])
      } else if (type == "runmean") {
        plots[[i]] <- ggRunmean(output, pars[i])
      }
      
    }
  }
  
  return(suppressWarnings(multiplot(plotlist = plots, cols = cols)))
}

