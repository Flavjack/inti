#' Figure with caption and notes
#'
#' Include figures with title and notes using a data base
#'
#' @param figure Path or URL of the figure.
#' @param caption Figure caption (default = NA).
#' @param notes Figure notes (default = NA).
#' @param label Label for the notes (default = NA).
#'
#' @return Figure with caption and notes
#' 
#' @export
#' 
#' @examples 
#' 
#' library(inti)
#' 
#' figure <- "https://inkaverse.com/reference/figures/logo.png"
#' 
#' figure %>% include_figure(caption = "Title test."
#'                         , notes = "Note test.")
#'

include_figure <- function(figure
                           , caption = NA
                           , notes = NA
                           , label = NA
                           ){
  
  title <- caption
  
  if(!is.na(notes)) {
    title <- paste(caption, notes)
    if(!is.na(label)) {
      title <- paste(caption, label, notes)
    }
  }
  
  fig <- figure %>% knitr::include_graphics()
  
# result ------------------------------------------------------------------

  list(caption = title
       , path = figure
       , figure = fig)

}

