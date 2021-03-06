% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggAutoCor.R
\name{ggAutoCor}
\alias{ggAutoCor}
\title{ggAutoCor}
\usage{
ggAutoCor(output, pars, conf, max.lag = NULL, min.lag = 0, title = "")
}
\arguments{
\item{conf}{The confidence level you want to see the limits for.}

\item{max.lag}{Maximum lag}

\item{min.lag}{Minimum lag}

\item{dat}{The vector of the paramater you want to see an autocorrelation plot for.}
}
\description{
Makes an autocorrealtion plot using ggplot2 - which looks nicer than the
core R plot functions.
}
