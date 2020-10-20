#' Table with footnotes
#'
#' Include tables with notes and titles for word and html documents
#'
#' @param data Data frame.
#' @param caption Table caption (default = NULL). See details.
#' @param notes Footnotes for the table (default = NULL). See details.
#' @param label Label for start the footnote (default = "Note:").
#' @param notation Notation for the symbols and footnotes (default =
#'   "alphabet"). See details.
#'
#' @details
#'
#' For \code{caption} and \code{notes} you can include {caption} and {notes} in
#' the last rows of your data frame. Where the information will be extracted
#' automatically for include in the formatted table. You can add the footnote
#' symbol using \code{{hypen}} in your table. \code{notation} could be use:
#' "alphabet", "number", "none".
#'
#' @return Table with caption and footnotes
#' 
#' @export
#'
#' @examples
#'
#' library(googlesheets4)
#' library(inti)
#' 
#' if (gs4_has_token()) {
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "1dfgpmCKdPmxRHozrZp0iE_xMGsvKTIcztDpMWYSEGaY")
#'
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' table <- gs %>%
#'     range_read("tab1")
#'
#' table %>% inti::include_table()
#'
#' }
#'

include_table <- function(data = NULL
                        , caption = NA
                        , notes = NA
                        , label = "Note:"
                        , notation = "alphabet"
) {

  # data <- table
  
  col_capt <- c("{caption}", "{title}", "{titulo}")
  col_note <- c("{notes}", "{note}", "{nota}", "{notas}")
  first_col <- names(data[1]) %>% as.symbol()
  col_list <- data[[first_col]]

  if(!is.null(data)){

    col_math <- col_list %in% col_capt
    col_cap <- col_list[col_math == TRUE]

    caption <- data %>%
      filter( {{first_col}} %in% {{col_cap}} ) %>%
      purrr::pluck(2)
    
    col_math <- col_list %in% col_note
    col_note <- col_list[col_math == TRUE]
    
    if (length(col_note) > 0) {
      
      notes <- data %>%
        filter( {{first_col}}  == {{col_note}} ) %>%
        pluck(2) 
      
    } else {notes <- NULL}
    
  }
  
  tab <-  data %>%
    filter( !{{first_col}}  %in% c( {{col_capt}}, {{col_note}} ) )
  
  if(!is.na(notes) && length(notes) > 1) {
    
    table <- tab %>% knitr::kable(caption = caption, format = "pipe") %>% 
      inti::footnotes(notes = notes, label = label, notation = notation)
    
  } else {
    
    table <-tab %>% knitr::kable(caption = caption, format = "pipe")
      
  }
  
return(table)

}
