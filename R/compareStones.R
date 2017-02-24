#' compareStones
#'
#' A function to extract the marginal likelihood from the stepping
#' stone sampler output of two BayesTraits runs, and work out the
#' log Bayes Factors.
#' @param comp The name of the stepping stones log file of the more complex of the two BayesTraits models.
#' @param simp The name of the stepping stone log file of the more simple of the two BayesTraits models.
#' @return The log Bayes factor of the comparison between the more complex and the more simple model.
#' @keywords hypothesis testing significance Bayes factors stepping stone
#' @export
#' @examples
#' compareStones("complex.model.stones.log.txt", "simple.model.stones.log.txt")

compareStones <- function(comp, simp) {
  raw.comp <- readLines(comp)
  raw.simp <- readLines(simp)

  marg.lh.comp <-  as.numeric(strsplit(raw.comp[length(raw.comp)], "\t")[[1]][2])
  marg.lh.simp <- as.numeric(strsplit(raw.simp[length(raw.simp)], "\t")[[1]][2])

  logBF <- 2 * (marg.lh.comp - marg.lh.simp)
  evidence <- vector()
  
  if (logBF < 2) {
    evidence <- "Weak evidence for complex model"
    } else if (logBF >= 2 && logBF < 5) {
      evidence <- "Positive evidence for complex model"
    } else if (logBF >= 5 && logBF <= 10) {
      evidence <- "Strong evidence for complex model"
    } else if (logBF > 10) {
      evidence <- "Very strong evidence for complex model"
    }
    
  names(logBF) <- "log BF"
  return(logBF)  
}

