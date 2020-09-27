#' Interactive data analysis
#'
#' Invoke RStudio addin to analyze and graph experimental design data
#' 
#' @param dependencies Install package dependencies for run the app
#'
#' @details
#'
#' Yupana: data analysis and graphics for experimental designs.
#'
#' @return Shiny app
#' 
#' @importFrom shiny runApp
#' 
#' @export
#'
#' @examples
#' 
#' if(interactive()){
#'
#'  inti::yupana()
#'
#' }
#' 

yupana <- function(dependencies = FALSE) {
  
  appDir <- system.file("yupanapro", package = "inti")
  
  if (appDir == "") {
    
    stop("Could not find example directory. Try re-installing `inti`."
         , call. = FALSE)
  }
  
  pkgs <- system.file("yupanapro/setup.r", package = "inti")
  
  if (dependencies == TRUE ) {
    
    source(pkgs)
    
  }

  shiny::runApp(appDir
                , display.mode = "normal"
                , launch.browser = TRUE
                , port = 1221
                )
}

