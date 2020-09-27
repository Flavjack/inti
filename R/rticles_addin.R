#' Build markdown o bookdown template
#'
#' Invoke RStudio addin to create markdown documents
#' 
#' @param dependencies Install package dependencies for run the app
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
#'  inti::rticles_addin()
#'
#' }
#'
#' @importFrom shiny runApp dialogViewer runGadget
#' 
#' @export
#'

rticles_addin <- function(dependencies = FALSE) {
  
  appDir <- system.file("rticles", package = "inti")
  
  if (appDir == "") {
    
    stop("Could not find example directory. Try re-installing `inti`."
         , call. = FALSE)
    
  }
  
  pkgs <- system.file("rticles/setup.r", package = "inti")
  
  if (dependencies == TRUE ) {
    
    source(pkgs)
    
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)

}

