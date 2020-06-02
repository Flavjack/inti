#' wanuy
#'
#' Fertilization management app.
#'
#' @importFrom shiny runApp
#' @export

wanuy <- function() {
    appDir <- system.file("wanuy", package = "inti")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `inti`.",
            call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}



