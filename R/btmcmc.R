#' btmcmc
#'
#' Returns the full mcmc object from a BayesTraits log file. This
#' is used inside plot functions and so on, but might be useful for
#' other MCMC manipulations and so on.
#' @param logfile The name of the logfile of the BayesTraits analysis.
#' @param thinning Thinning parameter for the posterior - defaults to 1 (all samples). 2 uses every second sample, 3 every third and so on.
#' @param burnin The number of generations to remove from the start of the chain as burnin. Use if the chain has not reached convergence before sampling began.
#' @return A data frame containing the sample from the BayesTrait mcmc.

btmcmc <- function(logfile, thinning = 1, burnin = 0) {

  raw <- readLines(logfile)
  model <- gsub(" ", "", raw[2])
  output <- do.call(rbind, strsplit(raw[grep("\\bIteration\\b", raw):length(raw)], "\t"))
  colnames(output) <- output[1, ]
  output <- output[c(2:nrow(output)), ]
  output <- data.frame(output, stringsAsFactors = FALSE)
 
  for (i in 1:ncol(output)) {
    if (colnames(output)[i] != "Model.string" && colnames(output)[i] != "Dep...InDep") {
      output[ ,i] <- as.numeric(output[ ,i])
    }
    
  }
  output <- output[seq.int(burnin, nrow(output), thinning), ]
  return(output)
}

