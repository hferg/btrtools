#' rjParamCount
#'
#' Counts the mean number of parameters in a vecotr of one or more RJ models. Simpler than the full localscalarPP since just the parameter number is considered.
#' @name rjParamCount
#' @param logs A vector of one or more rj output logfile names.
#' @param order Order the output. Logical - defaults to FALSE.
#' @param burnin If burnin is needed for the output, set it here.
#' @param thinning If the psoterior needs thinning, set it here.
#' @export

rjParamCount <- function(logs, burnin = 0, thinning = 1) {
  res <- matrix(ncol = 2, nrow = length(logs))
  colnames(res) <- c("logfile", "nParams")

  for (i in 1:length(logs)) {
    paramnums <- as.numeric(loadRJ(logs[[i]], burnin = burnin, thinning = thinning)$rj_output$No.Pram)
    res[i, 1] <- logs[[i]]
    res[i, 2] <- mean(paramnums)
  }
  res <- data.frame(res)
  res$nParams <- as.numeric(as.character(res$nParams))
  return(res)
}
