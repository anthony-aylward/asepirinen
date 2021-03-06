% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ase_29dec2014.R
\name{plot.gtm}
\alias{plot.gtm}
\title{Plot Grouped Tissue Model}
\usage{
\method{plot}{gtm}(res, y, title.text = NULL, print.counts = TRUE,
  cex.c = 1)
}
\arguments{
\item{res}{list, as returned by function 'gtm'}

\item{y}{mx2 matrix of allele read counts, col 1 for reference allele, col 2 for non ref allele allele, uses rownames as labels if present}

\item{title.text}{if present uses as a title in the plot}

\item{print.counts}{if TRUE, prints out the read counts in the form NONREF/ALL, if FALSE does not print counts}
}
\description{
a plot
}
