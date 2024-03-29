% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_folder.R
\name{split_folder}
\alias{split_folder}
\title{Split folder}
\usage{
split_folder(
  folder,
  export,
  units = "megas",
  size = 500,
  zip = TRUE,
  remove = FALSE
)
}
\arguments{
\item{folder}{Path of folder to split (path).}

\item{export}{Path to export the split folders (path).}

\item{units}{Units to split folder (string: "megas", "number").}

\item{size}{Folder size by the units selected (numeric).}

\item{zip}{Zip split folders (logical).}

\item{remove}{Remove  the split folder after zip (logical).}
}
\value{
zip files
}
\description{
Function to split folder by size or number of elements
}
\examples{

\dontrun{

split_folder("pictures/QUINOA 2018-2019 SC SEEDS EDWIN - CAMACANI/"
   , "pictures/split_num", remove = T, size = 400, units = "number")

}

}
