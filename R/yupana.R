#' Interactive data analysis
#'
#' Invoke RStudio addin to analyze and graph experimental design data
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
#' \dontrun{
#'
#'  inti::yupana()
#'
#' }
#' 

yupana <- function() {
  appDir <- system.file("yupanapro", package = "inti")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `inti`.",
         call. = FALSE)
  }

  shiny::runApp(appDir
                , display.mode = "normal"
                , launch.browser = TRUE
                , port = 1221
                )
}

