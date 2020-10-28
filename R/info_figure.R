#' Figure information
#'
#' Describe figures information for export or print
#'
#' @param caption Figure caption.
#' @param notes Figure notes.
#' @param url Figure url or link.
#' @param path Figure local path.
#' @param label Label for notes (default = NA)
#' @param nrows Free rows in the export table.
#'
#' @return data frame
#' 
#' @export
#' 
#' @examples 
#'
#' finfo <- info_figure(
#'   caption = "Figure caption."
#'   , notes = "Test note"
#'   , url = "www.image.dir"
#'   , label = "Source:"
#'   )
#' 

info_figure <- function(caption = NA
                        , notes = NA
                        , url = NA
                        , path = NA
                        , label = NA
                        , nrows = 50
                        ){
  
  tabinfo <- c("{caption}", "{notes}", "{url}", "{path}") %>% 
    enframe(value = "figure") %>% 
    select(!.data$name) %>% 
    tibble::add_column(description = c(caption, notes, url, path)) %>% 
    tibble::add_row(
      figure = rep_len(NA, nrows)
      , description = rep_len(NA, nrows)
      )
  
  if ( is.na(notes) ) { 
    
    cap <- caption 
    
  } else if (!is.na(notes)) {
    
    if (is.na(label)) {
      
      cap <- paste(caption, notes) 
      
    } else {
      
      cap <- paste(caption, label, notes)
      
    }
    
  }

  cap <- cap %>% gsub("\r?\n|\r", "", .)
  
# result ------------------------------------------------------------------

list(info = tabinfo, caption = cap)
  
}

