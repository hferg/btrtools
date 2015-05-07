#' btmcmc
#'
#' Returns the full mcmc object from a BayesTraits log file. This
#' is used inside plot functions and so on, but might be useful for
#' other MCMC manipulations and so on.
#' @param logfile The name of the logfile of the BayesTraits analysis.
#' @return A data frame containing the sample from the BayesTrait mcmc.

btmcmc <- function(logfile) {

  raw <- readLines(logfile)
  model <- gsub(" ", "", raw[2])
  output <- do.call(rbind, strsplit(raw[grep("\\bIteration\\b", raw):length(raw)], "\t"))
  colnames(output) <- output[1, ]
  output <- output[c(2:nrow(output)), ]
  output <- data.frame(output, stringsAsFactors = FALSE)
 
  for (i in 1:ncol(output)) {
    if (colnames(output)[i] != "Model.string") {
      output[ ,i] <- as.numeric(output[ ,i])
    }
    
  }
  return(output)
}

