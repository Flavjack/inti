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
#' tab <- tibble(
#' x = rep_len(1, 5)
#' , y = rep_len(3, 5)
#' , z = rep_len("c", 5)
#' )
#'
#' info <- tab %>% 
#' info_table(
#'   caption = "Figure caption"
#'   , notes = "test note"
#'   )
#'
#' info %>% inti::include_table()
#'
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
