#' Table information
#'
#' Add table information for include_table
#'
#' @param data data frame
#' @param caption Table caption.
#' @param notes Table notes.
#'
#' @return data frame
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

info_table <- function(data
                       , caption = NA
                       , notes = NA
                       ){
  
  # data <- tab
  
  tnames <- names(data) 
  
  col1 <- tnames[1] %>% as.symbol()
  col2 <- tnames[2] %>% as.symbol()
  
  tabinfo <- c("{caption}", "{notes}") %>% 
    enframe(value = {{col1}}) %>% 
    select(!.data$name) %>% 
    tibble::add_column({{col2}} := c(caption, notes))
  
  tab <- merge(data, tabinfo, all = TRUE, sort = F)
  
# results -----------------------------------------------------------------

list(table = tab, data = data)
  
}

