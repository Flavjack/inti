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
  
  if (dependencies == TRUE) {
    
    source("https://raw.githubusercontent.com/Flavjack/inti/master/inst/yupana/setup.R")
    
  }
  
  appDir <- system.file("yupana", package = "inti")
  
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

