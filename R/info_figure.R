#' Figure information
#'
#' Describe figures information for export or print
#'
#' @param caption Figure caption.
#' @param notes Figure notes.
#' @param url Figure url or link.
#' @param path Figure local path.
#' @param nrows Free rows in the export table.
#'
#' @return data frame
#' 
#' @export
#' 
#' @examples 
#'
#' finfo <- info_figure(
#'   caption = "Figure caption"
#'   , url = "www.image.dir"
#'   , nrows = 30
#'   )
#' 
#' 

info_figure <- function(caption = NA
                        , notes = NA
                        , url = NA
                        , path = NA
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
  
  tabinfo
  
}

