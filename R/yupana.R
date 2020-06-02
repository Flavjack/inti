#' yupana
#'
#' @description Data analysis for research experiments.
#' @author quipo.org
#' @importFrom shiny runApp
#' @export

yupana <- function() {
  appDir <- system.file("yupana", package = "inti")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `yupana`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


