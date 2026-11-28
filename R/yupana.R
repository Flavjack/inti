#' Interactive data analysis
#'
#' Invoke RStudio addin to analyze and graph experimental design data
#' 
#' @param dependencies Install package dependencies for run the app
#' @param port Port on which the app runs `[numeric: 1221]`. The default
#'   matches the port registered for Google sign-in. Use another value (or
#'   `NULL` for a random free port) if 1221 is busy, e.g. when TARPUY is
#'   already running; Google sign-in may not work on other ports.
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

yupana <- function(dependencies = FALSE, port = 1221) {
  
  if (dependencies == TRUE) {
    
    source("https://raw.githubusercontent.com/flozanoisla/inti/master/inst/yupana/setup.R")
    
  }
  
  appDir <- system.file("yupana", package = "inti")
  
  if (appDir == "") {
    
    stop("Could not find example directory. Try re-installing `inti`."
         , call. = FALSE)
  }
  
  shiny::runApp(appDir
                , display.mode = "normal"
                , launch.browser = TRUE
                , port = port
                )
  
}

