#' yupana
#'
#' @description Data analisys app for research experiments.
#' @family sapiens
#' @importFrom shiny runApp
#' @export

yupana <- function() {
  appDir <- system.file("fieldbook", package = "fieldbook")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `fieldbook`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


