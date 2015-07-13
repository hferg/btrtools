#' getPostStats
#'
#' A function to retuen the mean and SD of the posterior of one or more
#' parameters from a BayesTraits analysis.
#' @param logfile The name of the logfile from a BayesTraits analysis.
#' @param params A vector of one or more parameters of interests (e.g. a subset of getParams output)
#' @param thinning Thinning parameter for the posterior - defaults to 1 (all samples). 2 uses every second sample, 3 every third and so on.
#' @param burnin The number of generations to remove from the start of the chain as burnin.
#' @return A data frame of 3 columns, the parameter name, the mean of the parameter and its standard deviation.
#' @keywords posterior mean standard deviation
#' @export
#' @examples
#' params <- getParams("cool-data.log.txt")
#' getPostMean("cool-data.log.txt", params[c(2:5)]
#' getPostMean("cool-data.log.txt", "Lh")

getPostStats <- function(logfile, params, thinning = 1, burnin = 0) {

  modeStat <- function(x) {
    z <- unique(x)
    x[which.max(tabulate(match(x, z)))]
  }

  params.exc <- c("Model.string", "Tree.No", "No.Off.Parmeters", "No.Off.Zero")
  output <- btmcmc(logfile, thinning = thinning, burnin = burnin)
  params.tmp <- params[which(!params %in% params.exc)]
  res <- matrix(ncol = 5, nrow = length(params.tmp))
  colnames(res) <- c("param", "mean", "median", "mode", "sd")
  
  for (i in 1:length(params.tmp)) {
    print(params.tmp[i])
    res[i, 1] <- params.tmp[i]
    res[i, 2] <- round(mean(output[params.tmp[i]][ ,1]), 4)
    res[i, 3] <- round(median(output[params.tmp[i]][ ,1]), 4)
    res[i, 4] <- round(modeStat(output[params.tmp[i]][ ,1]), 4)
    res[i, 5] <- round(sd(output[params.tmp[i]][ ,1]), 4)
  }
  
  return(data.frame(res))
}

