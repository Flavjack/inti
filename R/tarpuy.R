#' Interactive fieldbook designs
#'
#' Invoke RStudio addin to create fieldbook designs
#' 
#' @param dependencies Install package dependencies for run the app
#'
#' @details
#'
#' Tarpuy allow to create experimental designs under an interactive app.
#'
#' @return Shiny app
#' 
#' @importFrom shiny runApp dialogViewer runGadget
#' 
#' @examples
#' 
#' if(interactive()){
#'
#'  inti::tarpuy()
#'
#' }
#' 
#' @export

tarpuy <- function(dependencies = FALSE) {

  if (dependencies == TRUE ) {
    
    source("https://raw.githubusercontent.com/Flavjack/inti/master/inst/tarpuy/setup.R")
    
  }
  
  appDir <- system.file("tarpuy", package = "inti")
  
  if (appDir == "") {
    
    stop("Could not find example directory. Try re-installing `inti`."
         , call. = FALSE)
    
  }

  shiny::runApp(appDir
                , display.mode = "normal"
                , launch.browser = TRUE
                , port = 1221
                )

}

