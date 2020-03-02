#' yupana
#'
#' @description Data analysis app for research experiments.
#' @family sapiens
#' @importFrom shiny runApp
#' @export

yupana <- function() {
  appDir <- system.file("yupana", package = "yupana")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `yupana`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


