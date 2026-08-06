#' Interactive fieldbook designs
#'
#' Invoke RStudio addin to create fieldbook designs
#'
#' @param dependencies Logical. If `TRUE`, install any missing TARPUY runtime
#'   dependencies using the local `inst/tarpuy/setup.R` script included with
#'   the installed `inti` package.
#'
#' @details
#'
#' TARPUY allows users to create experimental designs through an interactive
#' Shiny application.
#'
#' @return A Shiny application launched with [shiny::runApp()].
#'
#' @importFrom shiny runApp dialogViewer runGadget
#'
#' @examples
#'
#' if (interactive()) {
#'   inti::tarpuy()
#' }
#'
#' @export

tarpuy <- function(dependencies = FALSE) {

  if (isTRUE(dependencies)) {

    setup_file <- system.file(
      "tarpuy",
      "setup.R",
      package = "inti"
    )

    if (!nzchar(setup_file) || !file.exists(setup_file)) {
      stop(
        paste0(
          "Could not find the local TARPUY setup script in the installed ",
          "`inti` package. Try re-installing `inti`."
        ),
        call. = FALSE
      )
    }

    source(
      file = setup_file,
      local = TRUE,
      encoding = "UTF-8"
    )
  }

  appDir <- system.file("tarpuy", package = "inti")

  if (!nzchar(appDir) || !dir.exists(appDir)) {
    stop(
      "Could not find the TARPUY application directory. Try re-installing `inti`.",
      call. = FALSE
    )
  }

  shiny::runApp(
    appDir,
    display.mode = "normal",
    launch.browser = TRUE,
    port = 1221
  )
}
