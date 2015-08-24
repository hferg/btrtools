#' rjParamCount
#'
#' Counts the mean number of parameters in a vecotr of one or more RJ models. Simpler than the full localscalarPP since just the parameter number is considered.
#' @name rjParamCount
#' @param logs A vector of one or more rj output logfile names.
#' @param order Order the output. Logical - defaults to FALSE.
#' @param burnin If burnin is needed for the output, set it here.
#' @param thinning If the psoterior needs thinning, set it here.
#' @param mode Continuous or discrete.
#' @export

rjParamCount <- function(logs, burnin = 0, thinning = 1, mode) {
  res <- matrix(ncol = 3, nrow = length(logs))
  colnames(res) <- c("logfile", "meanParams", "sdParams")

  if (mode == "continuous") {

    for (i in 1:length(logs)) {
      paramnums <- as.numeric(loadRJ(logs[[i]], burnin = burnin, thinning = thinning)$rj_output$No.Pram)
      res[i, 1] <- logs[[i]]
      res[i, 2] <- mean(paramnums)
      res[i, 3] <- mean(paramnums)
    }
    res <- data.frame(res)
    res$nParams <- as.numeric(as.character(res$nParams))
  } else if (mode == "discrete") {
    for (i in 1:length(logs)) {
      raw <- readLines(logs[[i]])
      rawhead <- strsplit(raw[1:(grep("\\bIteration\\b", raw) -1)], "\t")
      rawtail <- strsplit(raw[grep("\\bIteration\\b", raw):length(raw)], "\t")
      names <- rawtail[[1]]
      rawtail <- rawtail[c(2:length(rawtail))]

      for (j in 1:length(rawtail)) {
        names(rawtail[[j]]) <- names
      }

      rawtail <- do.call(smartBind, rawtail)
      res[i, 1] <- logs[[i]]
      res[i, 2] <- mean(as.numeric(rawtail[ , "No Off Parmeters"]))
      res[i, 3] <- sd(as.numeric(rawtail[ , "No Off Parmeters"]))
    }
  }
  return(res)
}
