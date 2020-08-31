#' Field book experimental designs
#'
#' Invoke RStudio addin to create field book designs
#'
#' @details
#'
#' Tarpuy allow to create experimental designs under an interactive app.
#'
#' @return Shiny app
#'
#' @examples
#' \dontrun{
#'
#'  inti::tarpuy()
#'
#' }
#'
#' @author Flavio Lozano-Isla
#' @importFrom shiny runApp dialogViewer runGadget
#' @export

tarpuy <- function() {
  appDir <- system.file("tarpuy", package = "inti")
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


