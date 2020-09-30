#' Build markdown o bookdown template
#'
#' Invoke RStudio addin to create markdown documents
#' 
#' @details
#'
#' Create all the files in the present work directory. It is recommended use
#' ".Rproj" for bookdown documents. After create the ".Rproj" file. Open the
#' ".Rprj" and you should have the possibility to compile the book.
#'
#' @return Shiny app
#'
#' @examples
#' 
#' if(interactive()){
#'
#'  inti::rticles()
#'
#' }
#'
#' @importFrom shiny runApp dialogViewer runGadget
#' 
#' @export
#'

rticles <- function() {
  
  appDir <- system.file("rticles", package = "inti")
  
  if (appDir == "") {
    
    stop("Could not find example directory. Try re-installing `inti`."
         , call. = FALSE)
    
  }
  
  pkgs <- system.file("rticles/setup.r", package = "inti")
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)

}

