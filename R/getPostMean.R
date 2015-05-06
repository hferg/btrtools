#' getPostMean
#'
#' A function to retuen the mean and SD of the posterior of one or more
#' parameters from a BayesTraits analysis.
#' @param logfile The name of the logfile from a BayesTraits analysis.
#' @param params A vector of one or more parameters of interests (e.g. a subset of getParams output)
#' @return A data frame of 3 columns, the parameter name, the mean of the parameter and its standard deviation.
#' @keywords posterior mean standard deviation
#' @export
#' @examples
#' params <- getParams("cool-data.log.txt")
#' getPostMean("cool-data.log.txt", params[c(2:5)]
#' getPostMean("cool-data.log.txt", "Lh")

getPostMean <- function(logfile, params) {
  params.exc <- c("Model.string", "Tree.No", "No.Off.Parmeters", "No.Off.Zero")
  raw <- readLines(logfile)
  output <- do.call(rbind, strsplit(raw[grep("\\bIteration\\b", raw):length(raw)], "\t"))
  colnames(output) <- output[1, ]
  output <- output[c(2:nrow(output)), ]
  output <- data.frame(output, stringsAsFactors = FALSE)
  for (i in 1:ncol(output)) {
    if (colnames(output)[i] != "Model.string") {
      output[ ,i] <- as.numeric(output[ ,i])
    }
  }
  params.tmp <- params[which(!params %in% params.exc)]
  res <- matrix(ncol = 3, nrow = length(params.tmp))
  colnames(res) <- c("param", "mean", "sd")
  for (i in 1:length(params.tmp)) {
  print(i)
    res[i, 1] <- params.tmp[i]
    res[i, 2] <- mean(output[params.tmp[i]][ ,1])
    res[i, 3] <- sd(output[params.tmp[i]][ ,1])
  }
  return(data.frame(res))
}

