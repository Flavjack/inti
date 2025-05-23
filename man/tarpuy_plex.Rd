% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tarpuy_plex.R
\name{tarpuy_plex}
\alias{tarpuy_plex}
\title{Fieldbook plan information}
\usage{
tarpuy_plex(
  data = NULL,
  title = NULL,
  objectives = NULL,
  hypothesis = NULL,
  rationale = NULL,
  references = NULL,
  plan = NULL,
  institutions = NULL,
  researchers = NULL,
  manager = NULL,
  location = NULL,
  altitude = NULL,
  georeferencing = NULL,
  environment = NULL,
  start = NA,
  end = NA,
  about = NULL,
  fieldbook = NULL,
  project = NULL,
  repository = NULL,
  manuscript = NULL,
  album = NULL,
  nfactor = 2,
  design = "rcbd",
  rep = 3,
  zigzag = FALSE,
  nrows = NA,
  serie = 100,
  seed = 0,
  qrcode = "{fbname}{plots}{factors}"
)
}
\arguments{
\item{data}{Data with the fieldbook information.}

\item{title}{Project title.}

\item{objectives}{The objectives of the project.}

\item{hypothesis}{What are the expected results.}

\item{rationale}{Based in which evidence is planned the experiment.}

\item{references}{References.}

\item{plan}{General description of the project (M & M).}

\item{institutions}{Institutions involved in the project.}

\item{researchers}{Persons involved in the project.}

\item{manager}{Persons responsible of the collection of the data.}

\item{location}{Location of the project.}

\item{altitude}{Altitude of the experiment (m.a.s.l).}

\item{georeferencing}{Georeferencing information.}

\item{environment}{Environment of the experiment (greenhouse, lab, etc).}

\item{start}{The date of the start of the experiments.}

\item{end}{The date of the end of the experiments.}

\item{about}{Short description of the project.}

\item{fieldbook}{Name or ID for the fieldbook/project.}

\item{project}{link for project.}

\item{repository}{link to the repository.}

\item{manuscript}{link for manuscript.}

\item{album}{link with the photos of the project.}

\item{nfactor}{Number of factors for the design.}

\item{design}{Type of design.}

\item{rep}{Number of replication.}

\item{zigzag}{Experiment layout in zigzag [logic: F]}

\item{nrows}{Experimental design dimension by rows [numeric: value]}

\item{serie}{Number of digits in the plots.}

\item{seed}{Seed for the randomization.}

\item{qrcode}{Concatenate the QR code [string: \{fbname\}\{plots\}\{factors\}]}
}
\value{
data frame or list of arguments:

  \enumerate{ \item info \item variables \item design \item logbook \item
  timetable \item budget }
}
\description{
Information for build a plan for an experiment (PLEX)
}
\details{
Provide the information available.
}
