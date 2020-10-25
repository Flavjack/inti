#' Table with footnotes
#'
#' Include tables with notes and titles for word and html documents
#'
#' @param data Data frame.
#' @param caption Table caption (default = NULL). See details.
#' @param notes Footnotes for the table (default = NA). See details.
#' @param label Label for start the footnote (default = NA).
#' @param notation Notation for the symbols and footnotes (default =
#'   "none"). See details.
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
#' library(inti)
#'
#' tab <- data.frame(
#' x = rep_len(1, 5)
#' , y = rep_len(3, 5)
#' , z = rep_len("c", 5)
#' )
#'
#' info <- tab %>% 
#' info_table(
#'   caption = "Figure caption"
#'   , notes = "test note"
#'   , label = "Where:"
#'   )
#'
#' info %>% inti::include_table()
#'

include_table <- function(data = NULL
                        , caption = NA
                        , notes = NA
                        , label = NA
                        , notation = "none"
                        ) {

  # data <- info
  
  info <- data %>% pluck(1)
  
  col_capt <- c("{caption}", "{title}", "{titulo}")
  col_note <- c("{notes}", "{note}", "{nota}", "{notas}")
  col_label <- c("{label}", "{etiqueta}")
  col_notation <- c("{notation}", "{tipo}")
  first_col <- names(info[1]) %>% as.symbol()
  col_list <- info[[first_col]]

  if(!is.null(data)){

    col_math <- col_list %in% col_capt
    col_cap <- col_list[col_math == TRUE]

    caption <- info %>%
      filter( {{first_col}} %in% {{col_cap}} ) %>%
      purrr::pluck(2)
    
    col_math <- col_list %in% col_note
    col_note <- col_list[col_math == TRUE]
    
    if (length(col_note) > 0) {
      
      notes <- info %>%
        filter( {{first_col}}  == {{col_note}} ) %>%
        pluck(2) 
      
    } else {notes <- NA}
    
    col_math <- col_list %in% col_label
    col_label <- col_list[col_math == TRUE]
    
    if (length(col_label) > 0) {
      
      label <- info %>%
        filter( {{first_col}}  == {{col_label}} ) %>%
        pluck(2) 
      
    } else {label <- NA}
    
    col_math <- col_list %in% col_notation
    col_notation <- col_list[col_math == TRUE]
    
    if (length(col_notation) > 0) {
      
      notation <- info %>%
        filter( {{first_col}}  == {{col_notation}} ) %>%
        pluck(2) 
      
    } else {notation <- notation}
    
  }
  
  table <-  data %>% pluck(2)
  
  if (is.na(notes)) {
    
    ftab <- table %>% 
      knitr::kable(caption = caption, format = "pipe")
    
  } else {
    
    if(is.na(label)) { label <- ""}
    
    ftab <- table %>% 
      knitr::kable(caption = caption, format = "pipe") %>% 
      inti::footnotes(notes = notes, label = label, notation = notation)
    
  }
  
# result ------------------------------------------------------------------

return(ftab)

}
