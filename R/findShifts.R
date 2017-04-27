################################################################################
#' findShifts
#' 
#' Returns the node numbers of nodes that have been scaled in the posterior over the specified threshold. Works for rate scalars, and transformations. Also can return branch scalars, if rates. Only looks at the placement of scalars, not the cumulative downstream effects of scalars.
#' @param PP The psotprocessor (localscalrPP) output.
#' @param scalar The scalar to find and plot from the post processor - delta/lambda/kappa/node/branch. If rate then the nodes and branches that are given scalars over the threshold are returned.
#' @param threshold Threshold of probability in posterior to display deltas for, defaults to zero (i.e. shows all deltas shaded proportionally to the posterior probability)
#' @name findShifts
#' @export

findShifts <- function(PP, scalar, threshold) {
  if (scalar == "delta") {
    cl <- "nOrgnDelta"
    mode <- "trans"
  } else if (scalar == "kappa") {
    cl <- "nOrgnKappa"
    mode <- "trans"
  } else if (scalar == "lambda") {
    cl <- "nOrgnLambda"
    mode <- "trans"
  } else if (scalar == "branch") {
    cl <- "nOrgnBRate"
    mode <- "trans"
  } else if (scalar == "node") {
    cl <- "nOrgnNRate"
    mode <- "trans"
  } else if (scalar == "rate") {
    cl <- "nOrgnScalar"
    mode <- "rate"
  }

  if (mode == "rate") {
    output <- findRateShifts(PP, threshold)
  } else if (mode == "trans") {
    if (type == "branch") {
      stop("Branch scalars are not valid for transformations.")
    }
    output <- findTransShifts(PP, threshold, cl)
  }

  return(output)
}

################################################################################
#' findRateShifts
#' 
#' Return the node or branch numbers with rate scalars applied over the threshold number.
#' @param PP an output from rjpp
#' @param threshold the threshold over which scalars are considered interesting.
#' @param type Node or branch
#' @keywords internal
#' @name findRateShifts

findRateShifts <- function(PP, threshold, type) {

  if (threshold == 0) {
    threshold <- 1 / PP$niter
  }

  nodes <- PP$data$descNode[which((PP$data[ , "nOrgnNRate"] / PP$niter) >= threshold)]
  branches <- PP$data$descNode[which((PP$data[ , "nOrgnBRate"] / PP$niter) >= threshold)]
  edges <- which(tree$edge[ , 2] %in% branches)

  return(list(nodes = nodes, edges = edges))
}

################################################################################
#' findTransShifts
#' 
#' return the node numbers that have had transformations applied over a given threshold.
#' @param PP an output from rjpp
#' @param threshold The threshold over which transformations are considered "significant".
#' @keywords internal
#' @name findTransShifts

findTransShifts <- function(PP, threshold, cl) {
  if (threshold == 0) {
    threshold <- 1 / PP$niter
  } 
  
  nodes <- PP$data$descNode[which((PP$data[ , cl] / PP$niter) >= threshold)]
  return(nodes)
} 