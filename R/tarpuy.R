#' Fieldbook experimental designs
#'
#' Invoke RStudio addin to create field book designs
#' 
#' @param dependencies Install package dependencies for run the app
#'
#' @details
#'
#' Tarpuy allow to create experimental designs under an interactive app.
#'
#' @return Shiny app
#'
#' @examples
#' 
#' if(interactive()){
#'
#'  inti::tarpuy()
#'
#' }
#' 
#' @importFrom shiny runApp dialogViewer runGadget
#' 
#' @export

tarpuy <- function(dependencies = FALSE) {
  
  message("
  - If is the first time running Tarpuy consider install the app dependencies:
  > inti::tarpuy(dependencies = TRUE)
  - List of dependencies:
  > https://github.com/Flavjack/inti/blob/master/inst/tarpuy/setup.r 
  - Demo: https://inkaverse.com/articles/apps
          ")
  
  if (dependencies == TRUE ) {
    
    source("https://raw.githubusercontent.com/Flavjack/inti/master/inst/tarpuy/setup.r")
    
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

