#' getParams
#'
#' This functions returns the names of the estimated parameters from a BayesTraits
#' analysis logfile for input into plotting and other analysis functions.
#' @param logfile The name of the trait data file on which BayesTraits was run.
#' @return A vector of the parameter names of the model logfile results from.
#' @keywords parameters
#' @export
#' @examples
#' getParams("cool-data.txt")

getParams <- function(logfile) {
  raw <- readLines(logfile)
  params <- do.call(rbind, strsplit(raw[grep("\\bIteration\\b", raw):length(raw)], "\t"))[1, ]
  params <- params[c(2:length(params))]
  params <- chartr(" ()^-", ".....", params)
  return(params)
}

