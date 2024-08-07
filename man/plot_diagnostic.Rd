% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_diagnostic.R
\name{plot_diagnostic}
\alias{plot_diagnostic}
\title{Diagnostic plots}
\usage{
plot_diagnostic(data, formula, title = NA)
}
\arguments{
\item{data}{Experimental design data frame with the factors and traits.}

\item{formula}{Mixed model formula}

\item{title}{Plot title}
}
\value{
plots
}
\description{
Function to plot the diagnostic of models
}
\examples{

\dontrun{

library(inti)

plot_diagnostic(data = potato
                , formula = stemdw ~ (1|bloque) + geno*treat)

}

}
