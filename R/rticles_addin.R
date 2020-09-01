#' Build markdown o bookdown template
#'
#' Invoke RStudio addin to create markdown documents
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
#'
#'  inti::rticles_addin()
#'
#' }
#'
#' @author Flavio Lozano-Isla
#' 
#' @importFrom shiny runApp dialogViewer runGadget
#' 
#' @export

rticles_addin <- function() {
  appDir <- system.file("rticles", package = "inti")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `inti`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)

}


