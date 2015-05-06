#' plotPosts
#'
#' A function to plot one or more posterior distributions from the logfile of a BayesTraits
#' analysis.
#'
#' @param logfile The name of the trait data file on which BayesTraits was run.
#' @param params A vector containing the name of the parameter(s) that are to be plotted. Can be
#' subsetted from the output of getParams().
#' @param fill The colour for the bars of the histogram. If "count" histogram will be shaded according to count.
#' @param cols The number of columns to plot multiple plots into.
#' @keywords plot posterior histogram distribution
#' @export
#' @examples
#' plotPosterior(cool-data.txt, c("Lh", "Alpha 1"))
#' plotPosterior(cool-data.txt, params[c(1:2)])

plotPosts <- function(logfile, pars, fill = "dodgerblue", cols = 2) {
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
 
  if (length(pars) == 1) {
    bwidth <- 3.5 * sd(output[ ,pars]) * length(output[ ,pars]) ^ -(1/3)
 
    if (fill == "count") {
      ret <- ggplot(data.frame(p = output[ ,pars]), aes(x = p, fill = ..count..)) +
        geom_histogram(color = "darkgray", binwidth = bwidth) +
        scale_x_continuous(paste(pars))
    } else {
      ret <- ggplot(data.frame(p = output[ ,pars]), aes(x = p)) +
        geom_histogram(color = "darkgray", binwidth = bwidth, fill = fill) +
        scale_x_continuous(paste(pars))
    }
  return(ret)
  } else {
    plots <- list()
    for (i in 1:length(pars)) {
          bwidth <- 3.5 * sd(output[ ,pars[i]]) * length(output[ ,pars[i]]) ^ -(1/3)
      plots[[i]] <- ggplot(data.frame(p = output[ ,pars[i]]), aes(x = p)) +
        geom_histogram(color = "darkgray", binwidth = bwidth, fill = fill) +
        scale_x_continuous(paste(pars[i]))
    }
    return(multiplot(plotlist = plots, cols = cols))
  }
}

