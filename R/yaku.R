#' yaku
#'
#' Irrigation management app to calculate the water requirements according to
#' the dialy evapotranspiration.
#'
#' @importFrom shiny runApp
#' 

yaku <- function() {
    appDir <- system.file("yaku", package = "inti")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `inti`.",
            call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


