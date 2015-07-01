#' compPosts
#'
#' Compares histograms of one or more parameters from the same output file, or one parameter from one or more output files.
#'
#' @param logs The name of the trait data file on which BayesTraits was run, or a vector of >1 names if comparing between >1 logs.
#' @param pars A vector containing the names of two parameters to be compared. Must be a single parameter if comparing between two logs. Can 
#' be subsetted from the output of getParams().
#' @param thinning Thinning parameter for the posterior - defaults to 1 (all samples). 2 uses every second sample, 3 every third and so on.
#' @keywords plot posterior histogram distribution compare
#' @export
#' @examples
#' plotPosterior(cool-data.txt, c("Lh", "Alpha 1"))
#' plotPosterior(cool-data.txt, params[c(1:2)])

compPosts <- function(logs, pars, thinning = 1) {
  
  if (length(logs) == 1) {
    output <- btmcmc(logs, thinning = thinning)
  } else {
    output <- lapply(logs, btmcmc, thinning = thinning)
  }
  
  if (length(logs) == 1) {
    ps <- list()
    
    for (i in 1:length(pars)) {
      ps[[i]] <- data.frame(d = output[ ,pars[i]], id = pars[i])
    }
    
    p <- do.call(rbind, ps)
  } else {
    ps <- list()
    
    for (i in 1:length(output)) {
      ps[[i]] <- data.frame(d = output[[i]][ ,pars], id = logs[i])
    }
    
    p <- do.call(rbind, ps)
  }
  
  bwidth <- 3.5 * sd(p$d) * length(p$d) ^ -(1/3)
  ret <- ggplot(p, aes(x = d, fill = id)) +
    geom_histogram(binwidth = bwidth, alpha = 0.5, position = "identity")
  
  return(ret)
}

