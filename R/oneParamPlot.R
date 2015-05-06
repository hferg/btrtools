#' plotPosterior
#'
#' A function to plot one or more posterior distributions from the logfile of a BayesTraits
#' analysis.
#'
#' @param logfile The name of the trait data file on which BayesTraits was run.
#' @param params A vector containing the name of the parameter(s) that are to be plotted. Can be
#' subsetted from the output of getParams().
#' @keywords plot posterior histogram distribution
#' @export
#' @examples
#' plotPosterior(cool-data.txt, c("Lh", "Alpha 1"))
#' plotPosterior(cool-data.txt, params[c(1:2)])

plotPosterior <- function(logfile, params) {
  # A function to take the output of a BayesTraits run and plot the posterior of whichever
  # trait.
  # BayesTraits is the name of the text file that the analysis was conducted on.
  
  # Read in the data and extract the name of the model being used.
  raw <- readLines(logfile)
  model <- gsub(" ", "", raw[2])
  output <- do.call(rbind, strsplit(raw[grep("\\bIteration\\b", raw):length(raw)], "\t"))
  params <- output[1, ]
  colnames(output) <- params 
  output <- output[c(2:nrow(output)), ]
  output <- data.frame(output, stringsAsFactors = FALSE)
  for (i in 1:ncol(output)) {
    if (colnames(output)[i] != "Model.string") {
      output[ ,i] <- as.numeric(output[ ,i])
    }
  }
}






