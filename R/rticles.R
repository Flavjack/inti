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
#'  inti::rticles()
#'
#' }
#'
#' @importFrom shiny runApp dialogViewer runGadget
#' 
#' @export
#'

rticles <- function(dependencies = FALSE) {
  
  message("
  - If is the first time running Rticles consider install the app dependencies:
  > inti::rticles(dependencies = TRUE)
  - List of dependencies: 
  > https://github.com/Flavjack/inti/blob/master/inst/rticles/setup.R
  - Demo: https://inkaverse.com/articles/apps
          ")
  
  if (dependencies == TRUE ) {
    
    source("https://raw.githubusercontent.com/Flavjack/inti/master/inst/rticles/setup.R")
    
  }
  
  appDir <- system.file("rticles", package = "inti")
  
  if (appDir == "") {
    
    stop("Could not find example directory. Try re-installing `inti`."
         , call. = FALSE)
    
  }
  
  pkgs <- system.file("rticles/setup.r", package = "inti")
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)

}

