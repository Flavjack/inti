#' yupana
#'
#' Yupana es una aplicación web (online) interactiva multiplataforma de interfaz amigable e intuitiva; que busca agilizar el desarrollo de investigación y validación de productos en las instituciones del sector agropecuario e industrial.
#'
#' @references Lozano-Isla, Flavio, Gomez Carrion, Jimmy, Benites-Alfaro, Omar, & De Mendiburu, Felipe (2018). Yupana: Herramienta web interactiva para el análisis de datos en la investigación agropecuario e industrial. Quipo. <www.quipolab.com>
#'
#' @source
#'
#' Yupana .: <https://flavjack.shinyapps.io/yupana/>
#'
#' User Manual .: <https://flavjack.github.io/yupana-usm>
#'
#' @author quipo.org
#' @importFrom shiny runApp
#' @export

yupana <- function() {
  appDir <- system.file("yupana", package = "inti")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `inti`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


