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
  
  appDir <- system.file("tarpuy", package = "inti")
  
  if (appDir == "") {
    
    stop("Could not find example directory. Try re-installing `inti`."
         , call. = FALSE)
    
  }
  
  pkgs <- system.file("tarpuy/setup.r", package = "inti")
  
  if (dependencies == TRUE ) {
    
    source(pkgs)
    
  }

  shiny::runApp(appDir
                , display.mode = "normal"
                , launch.browser = TRUE
                , port = 1221
                )

}

