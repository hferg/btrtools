#' getStones
#'
#' Get the marginal likelihoods from a single, or vector of, stepping stones log files.
#' @param logs A vector of one or more log files from stepping stones analysis.
#' @name getStones
#' @export

getStones <- function(logs, order = TRUE, paramcount = FALSE) {
  res <- matrix(ncol = 2, nrow = length(logs))
  colnames(res) <- c("logfile", "marginalLh")
  for (i in 1:length(logs)) {
    raw <- readLines(logs[[i]])
    res[i, 1] <- logs[[i]]
    res[i, 2] <- as.numeric(strsplit(raw[length(raw)], "\t")[[1]][2])
  }
  res <- data.frame(res)
  res$marginalLh <- as.numeric(as.character(res$marginalLh))

  if (order) {
    res <- res[order(res$marginalLh, decreasing = TRUE), ]
  }

  return(res)
}
