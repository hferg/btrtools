#' zeroTest
#'
#' Takes a parameter and plots the posterior as a histogram with values >0 one
#' colour and values <0 another. If plot is turned off a table is returned
#' instead, which has, in decimal form, the fraction of values <0 and the 
#' fraction >0.
#' @param logfile The name of the BayesTraits output logfile.
#' @param pars A vector of parameters to plot/summarise
#' @param cols The number of columns to plot into (if number of pars >1)
#' @param value The value against which to test (defaults to zero)
#' @param plot If true, a histogram coloured according to value is plotted. If false, a table summarising the posterior with respect to value is printed to screen.
#' @param thinning Thinning parameter for the posterior - defaults to 1 (all samples). 2 uses every second sample, 3 every third and so on.
#' @param burnin The number of generations to remove from the start of the chain as burnin.
#' @export
#' @keywords significance test testing posterior
#' @examples
#' zeroTest("cool-data.log", "Alpha")
#' zeroTest("cool-data.log", c("Alpha", "Beta.2"))
#' zeroTest("cool-data.log", "Lambda", value = 0.5)
#' zeroTest("cool-data.log", "Lambda", value = 0.5, plot = FALSE)

zeroTest <- function(logfile, pars, cols = 2, plot = TRUE, value = 0, thinning = 1, 
  burnin = 0) {
  if (is.data.frame(logfile)) {
    output <- logfile
  } else { 
    output <- btmcmc(logfile, thinning = thinning, burnin = burnin)
  }

  if (plot == TRUE) {
    
    if (length(pars) == 1) {
      plot.cols <- c("orangered", "dodgerblue")
      names(plot.cols) <- c(paste("< ", value), paste(">= ", value))
      bwidth <- 3.5 * sd(output[ ,pars]) * length(output[ ,pars]) ^ -(1/3)
      p <- data.frame(p = output[ ,pars], z = output[ ,pars] >= value)
      colnames(p) <- c(pars, "z")
      p$z[which(p[ ,pars] < value)] <- paste("< ", value)
      p$z[which(p[ ,pars] >= value)] <- paste(">= ", value)
      ret <- ggplot(p, aes_string(x = pars, fill = "z")) +
        geom_histogram(colour = "darkgray", binwidth = bwidth) +
        scale_fill_manual(name = "", values = plot.cols)
      return(ret)
    } else {
      plots <- list()

      if (length(value) == 1) {
        value <- rep(value, length(pars))
      }

      if (length(value) != length(pars)) {
        stop("The length of the values vector must equal the number of parameters.")
      }
      
      for (i in 1:length(pars)) {
        plot.cols <- c("orangered", "dodgerblue")
        names(plot.cols) <- c(paste("< ", value[i]), paste(">= ", value[i]))
        bwidth <- 3.5 * sd(output[ ,pars[i]]) * length(output[ ,pars[i]]) ^ -(1/3)
        p <- data.frame(p = output[ ,pars[i]], z = NA)
        colnames(p) <- c(pars[i], "z")
        p$z[which(p[ ,pars[i]] < value[i])] <- paste("< ", value[i])
        p$z[which(p[ ,pars[i]] >= value[i])] <- paste(">= ", value[i])
        plots[[i]] <- ggplot(p, aes_string(x = pars[i], fill = "z")) +
          geom_histogram(colour = "darkgrey", binwidth = bwidth) +
          scale_fill_manual(name = "", values = plot.cols)
      }
            
    return(suppressWarnings(multiplot(plotlist = plots, cols = cols)))
    }
  } else {
    res <- matrix(ncol = 3, nrow = length(pars))
    colnames(res) <- c(paste(">= ", value), paste("< ", value), paste(">= ", value, "/ < ", value))
    rownames(res) <- pars
    for (i in 1:length(pars)) {
      res[i, 1] <- sum(output[ ,pars[i]] >= value)
      res[i ,2] <- sum(output[ ,pars[i]] < value)
      res[i ,3] <- sum(output[ ,pars[i]] >= value) / sum(output[ ,pars[i]] < value)
    }
    
  return(suppressWarnings(res))
  }
}

