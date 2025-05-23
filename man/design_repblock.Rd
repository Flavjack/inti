% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/design_repblock.R
\name{design_repblock}
\alias{design_repblock}
\title{Experimental design in CRD and RCBD}
\usage{
design_repblock(
  nfactors = 1,
  factors,
  type = "crd",
  rep = 3,
  zigzag = FALSE,
  nrows = NA,
  serie = 100,
  seed = NULL,
  fbname = "inkaverse",
  qrcode = "{fbname}{plots}{factors}"
)
}
\arguments{
\item{nfactors}{Number of factor in the experiment [numeric: 1].}

\item{factors}{Lists with names and factor vector [list].}

\item{type}{Type of experimental arrange [string: "crd" "rcbd" "lsd"]}

\item{rep}{Number of replications in the experiment [numeric: 3].}

\item{zigzag}{Experiment layout in zigzag [logic: F].}

\item{nrows}{Experimental design dimension by rows [numeric: value]}

\item{serie}{Number to start the plot id [numeric: 100].}

\item{seed}{Replicability from randomization [numeric: NULL].}

\item{fbname}{Bar code prefix for data collection [string: "inkaverse"].}

\item{qrcode}{Concatenate the QR code [string: \{fbname\}\{plots\}\{factors\}]}
}
\value{
A list with the field-book design and parameters
}
\description{
Function to deploy field-book experiment for CRD and RCBD
}
\examples{

\dontrun{

library(inti)

factores <- list("geno" = c("A", "B", "C", "D", "D", 1, NA, NA, NULL, "NA")
                 , "salt stress" = c(0, 50, 200, 200, "T0", NA, NULL, "NULL")
                 , time = c(30, 60, 90)
                 )

fb <-design_repblock(nfactors = 2
                     , factors = factores
                     , type = "rcbd"
                     , rep = 5
                     , zigzag = T
                     , seed = 0
                     , nrows = 20
                     , qrcode = "{fbname}{plots}{factors}"
                     )
                     
dsg <- fb$fieldbook

fb \%>\%   
  tarpuy_plotdesign(fill = "plots") 

fb$parameters

}
}
