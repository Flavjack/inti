#' Build markdown o bookdown template
#'
#' Invoke RStudio add-in to create markdown documents
#'
#' @details
#'
#' Create all the files in the present work directory.
#' It is recommended use ".Rproj" for bookdown documents.
#' After create the ".Rproj" file.
#' Open the ".Rprj" and you should have the possibility to compile the book.
#'
#' @return Shiny app
#'
#' @examples
#' \dontrun{
#'  rticles()
#' }
#'
#' @author Flavio Lozano-Isla
#' @importFrom shiny runApp dialogViewer runGadget
#' @export

rticles <- function() {
  appDir <- system.file("rstudio", package = "inti")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `inti`.",
         call. = FALSE)
  }

  viewer <- dialogViewer("lozanoisla.com", width = 500, height = 450)

  runGadget(runApp(appDir), viewer =  viewer)
}


