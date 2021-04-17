#' Table with footnotes
#'
#' Include tables with title and footnotes for word and html documents
#'
#' @param table Data frame.
#' @param caption Table caption (default = NULL). See details.
#' @param notes Footnotes for the table (default = NA). See details.
#' @param label Label for start the footnote (default = NA).
#' @param notation Notation for the symbols and footnotes (default =
#'   "none") Others: "alphabet", "number", "symbol".
#'
#' @return Table with caption and footnotes
#' 
#' @export
#'
#' @examples
#' 
#' library(inti)
#'
#' table <- data.frame(
#' x = rep_len(1, 5)
#' , y = rep_len(3, 5)
#' , z = rep_len("c", 5)
#' )
#'
#' table %>% inti::include_table(
#'   caption = "Title caption"
#'   , notes = "Footnote"
#'   , label = "Source:"
#'   )
#'   

include_table <- function(table
                        , caption = NA
                        , notes = NA
                        , label = NA
                        , notation = "none"
                        ) {
  
  if(!is.data.frame(table)) stop("Use a data frame or table")
  
  ftab <- table %>% 
    knitr::kable(caption = caption
                 , format = "pipe")
  
  if (!is.na(notes)) {
    
    ftab <- ftab %>% 
      inti::footnotes(notes = notes
                      , label = label
                      , notation = notation)
  }
  
# result ------------------------------------------------------------------

return(ftab)

}
